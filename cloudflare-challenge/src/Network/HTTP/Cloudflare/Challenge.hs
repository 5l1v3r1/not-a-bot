{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Network.HTTP.Cloudflare.Challenge
    ( throughChallenge
    , solveChallenge

    , CloudflareChallengeException(..)
    , CloudflareChallengeFlowException(..)
    , CloudflareChallengePageException(..)

    , GenericClient(..)
    ) where

import Prelude hiding (takeWhile)

import Control.Concurrent (threadDelay)
import Control.Exception (Exception)
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.Acquire
import Data.Attoparsec.Text
import Data.Function (fix)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8')
import Data.Typeable (Typeable)
import Network.HTTP.Types
import System.Exit (ExitCode(..))
import Text.XML.Lens

import Data.Conduit
import Data.Conduit.Binary (isolate)
import Data.Conduit.Combinators (sinkLazy)
import Data.Conduit.Process (proc, sourceProcessWithStreams)

import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.Text as T


data GenericClient m resp = GenericClient
    { clientStatus :: resp -> Status
    , clientHost   :: resp -> T.Text
    , clientHTML   :: resp -> m (Maybe Document)
    , clientRefer  :: resp -> ([T.Text], QueryText) -> Acquire resp
    }

-- According to the natural HTTP/HTML flow, the original request method is lost
-- during the course of a Cloudflare challenge (and ultimately replaced with a
-- get). This doesn't cause issues in the browser, because it's unlikely that
-- you would end up on a page that will result in a request other than a GET
-- without already having been granted access through Cloudflare.
--
-- TODO: Provide the following flow as an option: if presented with a challenge,
-- solve it but don't follow the resulting redirect.

throughChallenge :: (MonadError (CloudflareChallengeException (ReleaseKey, resp)) m, MonadResource m)
                 => GenericClient m resp
                 -> Maybe Int
                 -> Acquire resp
                 -> m (ReleaseKey, resp)
throughChallenge client maxChallenges = wrapped
  where
    wrapped = case maxChallenges of
        Nothing -> fix (go False)
        Just lim ->
            let limited 0 = fix (go True)
                limited n = go False (limited (n - 1))
            in limited lim
    go annoyed cont req = do
        (key, resp) <- allocateAcquire req
        let giveUp ex = throwError (CloudflareChallengeFlowException ex (key, resp))
        case (statusCode (clientStatus client resp), annoyed) of
            (403, _   ) -> giveUp CloudflareChallengeFlowCaptcha
            (503, True) -> giveUp CloudflareChallengeFlowLoop
            (503, _   ) -> do
                doc <- liftMaybe (CloudflareChallengePageException CloudflareChallengePageInvalidHTML) =<<
                    clientHTML client resp
                release key
                (path, timeout) <- liftEitherWith CloudflareChallengePageException =<<
                    (liftIO (solveChallenge (clientHost client resp) doc))
                liftIO $ threadDelay (1000 * (timeout + 300))
                cont $ clientRefer client resp path
            _ -> return (key, resp)


solveChallenge :: T.Text -> Document -> IO (Either CloudflareChallengePageException (([T.Text], QueryText), Int))
solveChallenge host doc = runExceptT $ do
    (input, output, timeout) <- liftMaybe CloudflareChallengePageParseFailure $
        extractChallenge doc host
    sol <- withExceptT (maybe CloudflareChallengePageNodeExitUnexpectedOutput CloudflareChallengePageNodeExitFailure) $
        runChallengeCode input
    return (output sol, timeout)

extractChallenge :: Document -> T.Text -> Maybe (T.Text, T.Text -> ([T.Text], QueryText), Int)
extractChallenge doc host = do
    script   <- get $ named "script" . nodes . ix 0 . _Content
    jschl_vc <- get $ attributeIs "name" "jschl_vc"       . attr "value"
    pass     <- get $ attributeIs "name" "pass"           . attr "value"
    rawPath  <- get $ attributeIs "id"   "challenge-form" . attr "action"
    path            <- eitherToMaybe $ parseOnly parsePath rawPath
    (code, timeout) <- eitherToMaybe $ parseOnly (parseChallengeCode host) script
    let mkPath answer = (,) path
            [ ("jschl_vc", Just jschl_vc)
            , ("pass", Just pass)
            , ("jschl_answer", Just answer)
            ]
    return (code, mkPath, timeout)
  where
    get sel = doc ^? root . entire . sel

parsePath :: Parser [T.Text]
parsePath = many1 (char '/' *> takeWhile (inClass "a-zA-Z0-9_-")) <* endOfInput

parseChallengeCode :: T.Text -> Parser (T.Text, Int)
parseChallengeCode host = do
    manyTill line "setTimeout(function(){" >> line
    vars <- line
    manyTill line (char ';')
    math <- line
    let code = vars <> "var a = {}, t = '" <> host <> "';" <> math <> "; a.value"
    takeTill (== '}') >> "}, "
    timeout <- decimal
    return $ (code, timeout)
  where
    line = takeWhile (not . isEndOfLine)  <* skipSpace

runChallengeCode :: T.Text -> ExceptT (Maybe Int) IO T.Text
runChallengeCode code = do
    (exitCode, stdout, _) <- lift $ sourceProcessWithStreams
        (proc "node" ["-e", expr])
        (yield (encodeUtf8 code))
        (isolate 4096 .| sinkLazy)
        (return ())
    case exitCode of
        ExitFailure n -> throwError $ Just n
        ExitSuccess -> liftMaybe Nothing $
            eitherToMaybe (decodeUtf8' stdout)
                >>= ATL.maybeResult . ATL.parse (fst <$> match scientific)
  where
    expr = "var chunks = []; process.stdin.resume(); process.stdin.on('data', function(chunk) { chunks.push(chunk); }); process.stdin.on('end', function() { console.log(require('vm').runInNewContext(Buffer.concat(chunks), Object.create(null), { timeout: 5000 })); });"


eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e = maybe (throwError e) return

liftEitherWith :: MonadError e m => (e' -> e) -> Either e' a -> m a
liftEitherWith f = either (throwError . f) return


-- Exceptions

data CloudflareChallengeException resp =
      CloudflareChallengeFlowException CloudflareChallengeFlowException resp
    | CloudflareChallengePageException CloudflareChallengePageException
    deriving (Show, Typeable, Functor)

data CloudflareChallengeFlowException =
      CloudflareChallengeFlowCaptcha
    | CloudflareChallengeFlowLoop
    deriving Show

data CloudflareChallengePageException =
      CloudflareChallengePageInvalidHTML
    | CloudflareChallengePageParseFailure
    | CloudflareChallengePageNodeExitFailure Int
    | CloudflareChallengePageNodeExitUnexpectedOutput
    deriving Show

instance (Show resp, Typeable resp) => Exception (CloudflareChallengeException resp)
instance Exception CloudflareChallengeFlowException
instance Exception CloudflareChallengePageException
