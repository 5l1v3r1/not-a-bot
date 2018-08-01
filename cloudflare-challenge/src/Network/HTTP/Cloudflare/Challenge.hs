{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.HTTP.Cloudflare.Challenge
    (
      faceCloudflareChallenge'

    , faceCloudflareChallenge
    , withFaceCloudflareChallenge
    , faceCloudflareChallengeConsume

    , faceCloudflareChallenge_
    , withFaceCloudflareChallenge_
    , faceCloudflareChallengeConsume_

    , HistoriedCloudflareException(..)
    , CloudflareException(..)
    , FlowException(..)
    , ChallengeException(..)
    , NodeException(..)

    , solveChallenge

    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, Exception, throwIO)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Monad.Writer
import Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteString.Builder (toLazyByteString)
import Data.Conduit
import Data.Conduit.Binary (isolate)
import Data.Conduit.Process (sourceProcessWithStreams, proc)
import Data.Conduit.Text (encode, utf8)
import Data.Function (fix)
import Data.Maybe
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.Lens
import Network.HTTP.Types
import System.Exit (ExitCode(..))
import Text.HTML.DOM (sinkDoc)
import Text.XML.Lens
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.Text.Lazy as AT
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T


faceCloudflareChallenge' :: (MonadWriter [HistoriedResponse L.ByteString] m, MonadError CloudflareException m, MonadResource m)
                         => Maybe Int
                         -> (Request -> IO (HistoriedResponse BodyReader))
                         -> Request
                         -> m (ReleaseKey, HistoriedResponse BodyReader)
faceCloudflareChallenge' max503s exec = wrapped . ensureCookieJar
  where
    wrapped = case max503s of
        Nothing -> fix (go False)
        Just max ->
            let limited 0 = fix (go True)
                limited n = go False (limited (n - 1))
            in limited max
    go annoyed cont req = do
        (key, resp) <- allocate (exec req) (responseClose . hrFinalResponse)
        case statusCode . responseStatus $ hrFinalResponse resp of
            200 -> return (key, resp)
            oth -> do
                body <- liftIO $ L.fromChunks <$> brConsume (responseBody (hrFinalResponse resp))
                release key
                tell [resp & _hrFinalResponse . _responseBody .~ body]
                case (oth, annoyed, method req == methodGet) of
                    (403, _   , _    ) -> throwError $ CloudflareFlowException FlowCaptcha
                    (503, True, _    ) -> throwError $ CloudflareFlowException FlowLoop
                    (503, _   , False) -> throwError $ CloudflareFlowException (FlowChallengeButMethodNotGet (method req))
                    (503, _   , _    ) -> do
                        doc <- liftEither . first (CloudflareChallengeException . ChallengeHTMLParseException)
                            $ C.sourceLazy body `connect` sinkDoc
                        ((path, query), timeout) <- liftEither . first CloudflareChallengeException
                            =<< liftIO (solveChallenge doc . B.length . host $ hrFinalRequest resp)
                        liftIO $ threadDelay (1000 * (timeout + 300))
                        cont $ req
                            & _path .~ path
                            & _queryString .~ query
                            & _requestHeaders %~ setHeader hReferer (asReferer resp)
                            & _cookieJar .~ Just (responseCookieJar (hrFinalResponse resp))
                    _ -> throwError $ CloudflareFlowException (FlowUnexpectedStatusCode oth)

solveChallenge :: Document -> Int -> IO (Either ChallengeException ((B.ByteString, B.ByteString), Int))
solveChallenge doc hostLen = case prepareChallenge doc hostLen of
    Nothing -> return $ Left ChallengeParseException
    Just (input, output, timeout) ->
        first ChallengeNodeException . second ((, timeout) . output) <$> runChallengeCode input

prepareChallenge :: Document -> Int -> Maybe (T.Text, T.Text -> (B.ByteString, B.ByteString), Int)
prepareChallenge doc hostLen = do
    script   <- get $ named "script" . nodes . ix 0 . _Content
    jschl_vc <- get $ attributeIs "name" "jschl_vc"       . attr "value"
    pass     <- get $ attributeIs "name" "pass"           . attr "value"
    path     <- get $ attributeIs "id"   "challenge-form" . attr "action"
    '/'      <- fst <$> uncons path
    (precode, timeout) <- eitherToMaybe $ parseOnly parseChallengeCode script
    let code = precode <> " + " <> T.pack (show hostLen)
        mkPath answer = (,) (encodeUtf8 path) . L.toStrict . toLazyByteString $ renderQueryText True
            [ ("jschl_vc", Just jschl_vc)
            , ("pass", Just pass)
            , ("jschl_answer", Just answer)
            ]
    return (code, mkPath, timeout)
  where
    get sel = doc ^? root . entire . sel

parseChallengeCode :: Parser (T.Text, Int)
parseChallengeCode = do
    manyTill line ("var s,t,o,p,b,r,e,a,k,i,n,g,f, ")
    foo <- line
    manyTill line ";"
    bar <- T.pack <$> manyTill anyChar "a.value = "
    baz <- takeTill (== ' ')
    takeTill (== '}') >> "}, "
    timeout <- decimal
    takeLazyText
    return $ (foo <> bar <> baz, timeout)
  where
    line = A.takeWhile (not . isEndOfLine)  <* skipSpace

runChallengeCode :: T.Text -> IO (Either NodeException T.Text)
runChallengeCode code = do
    (exitCode, stdout, stderr) <- sourceProcessWithStreams
        (proc "node" ["-e", expr])
        (yield code .| encode utf8)
        (isolate 4096 .| C.sinkLazy)
        (isolate 4096 .| C.sinkLazy)
    return $ case exitCode of
        ExitFailure n -> Left $ NodeExitFailure n stderr
        ExitSuccess -> maybe
            (Left $ NodeUnexpectedOutput stdout)
            (Right . decodeUtf8)
            (AL.maybeResult $ AL.parse (fst <$> AC.match AC.scientific) stdout)
  where
    expr = "var chunks = []; process.stdin.resume(); process.stdin.on('data', function(chunk) { chunks.push(chunk); }); process.stdin.on('end', function() { console.log(require('vm').runInNewContext(Buffer.concat(chunks), Object.create(null), { timeout: 5000 })); });"


maxBodySize :: Int
maxBodySize = 16 * 4096

setHeader :: HeaderName -> B.ByteString -> [Header] -> [Header]
setHeader h v = (:) (h, v) . filter ((/=) h . fst)

asReferer :: HistoriedResponse body -> B.ByteString
asReferer = requestUrl . hrFinalRequest

requestUrl :: Request -> B.ByteString
requestUrl = scheme <> host <> (p <$> secure <*> port) <> path <> queryString
  where
    scheme = bool "https://" "http://" <$> secure
    p True 443 = ""
    p False 80 = ""
    p _ oth = ":" <> B.pack (show oth)

ensureCookieJar :: Request -> Request
ensureCookieJar = _cookieJar %~ Just . fromMaybe (createCookieJar [])

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just


-- Exceptions

data HistoriedCloudflareException = HistoriedCloudflareException
    { cloudflareException :: CloudflareException
    , cloudflareExceptionHistory :: [HistoriedResponse L.ByteString]
    } deriving Show

instance Exception HistoriedCloudflareException

data CloudflareException =
      CloudflareFlowException FlowException
    | CloudflareChallengeException ChallengeException
    | CloudflareNodeException NodeException
    deriving Show

data FlowException =
      FlowCaptcha
    | FlowLoop
    | FlowUnexpectedStatusCode Int
    | FlowChallengeButMethodNotGet Method
    deriving Show

data ChallengeException =
      ChallengeParseException
    | ChallengeHTMLParseException SomeException
    | ChallengeNodeException NodeException
    deriving Show

data NodeException =
      NodeExitFailure Int L.ByteString
    | NodeUnexpectedOutput L.ByteString
    deriving Show


-- Convenience

defaultMax503s :: Maybe Int
defaultMax503s = Just 10

faceCloudflareChallenge_ :: (Request -> IO (HistoriedResponse BodyReader)) -> Request -> ResourceT IO (ReleaseKey, Response BodyReader)
faceCloudflareChallenge_ exec req = do
    result <- faceCloudflareChallenge exec req
    case result of
        (Left ex, hist) -> throwM $ HistoriedCloudflareException ex hist
        (Right resp, _) -> return $ second hrFinalResponse resp

faceCloudflareChallenge :: (Request -> IO (HistoriedResponse BodyReader)) -> Request -> ResourceT IO (Either CloudflareException (ReleaseKey, HistoriedResponse BodyReader), [HistoriedResponse L.ByteString])
faceCloudflareChallenge exec req = runWriterT . runExceptT $ faceCloudflareChallenge' defaultMax503s exec req

withFaceCloudflareChallenge_ :: (Request -> IO (HistoriedResponse BodyReader)) -> Request -> (Response BodyReader -> IO a) -> IO a
withFaceCloudflareChallenge_ exec req f = runResourceT $ faceCloudflareChallenge_ exec req >>= lift . f . snd

withFaceCloudflareChallenge :: (Request -> IO (HistoriedResponse BodyReader)) -> Request -> ((Either CloudflareException (HistoriedResponse BodyReader), [HistoriedResponse L.ByteString]) -> IO a) -> IO a
withFaceCloudflareChallenge exec req f = runResourceT $ faceCloudflareChallenge exec req >>= lift . f . first (second snd)

faceCloudflareChallengeConsume_ :: (Request -> IO (HistoriedResponse BodyReader)) -> Request -> IO (Response L.ByteString)
faceCloudflareChallengeConsume_ exec req = withFaceCloudflareChallenge_ exec req $ _responseBody %%~ fmap L.fromChunks . brConsume

faceCloudflareChallengeConsume :: (Request -> IO (HistoriedResponse BodyReader)) -> Request -> IO (Either CloudflareException (HistoriedResponse L.ByteString), [HistoriedResponse L.ByteString])
faceCloudflareChallengeConsume exec req = withFaceCloudflareChallenge exec req $ _1 . _Right . _hrFinalResponse . _responseBody %%~ fmap L.fromChunks . brConsume
