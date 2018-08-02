{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Client.Cloudflare.Challenge
    ( throughChallenge
    , throughChallengeWithLogging
    , withThroughChallenge
    , withThroughChallengeWithLogging
    , withThroughChallengeSimple
    , withThroughChallengeWithLoggingSimple
    , HistoriedCloudflareChallengeException(..)
    ) where

import Network.HTTP.Cloudflare.Challenge hiding (throughChallenge)
import qualified Network.HTTP.Cloudflare.Challenge as CF

import Control.Exception (Exception)
import Control.Lens
import Control.Monad.Catch.Pure
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Monad.Writer
import Data.Acquire
import Data.Bool (bool)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Conduit
import Data.Conduit.Binary (isolate)
import Data.Conduit.Combinators (sinkLazy)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.Lens
import Network.HTTP.Types
import Text.HTML.DOM (sinkDoc)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T


client :: MonadIO m => Maybe Int -> Manager -> GenericClient m (HistoriedResponse BodyReader)
client maxBodySize man = (clientCommon man)
    { clientHTML = \resp -> fmap (either (const Nothing) Just) . runCatchT $
        bodyReaderSource (responseBody (hrFinalResponse resp)) `connect` maybe id (fuse . isolate) maxBodySize sinkDoc
    }

clientWithLogging :: MonadIO m => (HistoriedResponse L.ByteString -> m ()) -> Maybe Int -> Manager -> GenericClient m (HistoriedResponse BodyReader)
clientWithLogging log maxBodySize man = (clientCommon man)
    { clientHTML = \resp -> do
        resp' <- readBody maxBodySize resp
        log resp'
        fmap (either (const Nothing) Just) . runCatchT $
            C.sourceLazy (responseBody (hrFinalResponse resp')) `connect` sinkDoc
    }

clientCommon :: Manager -> GenericClient m (HistoriedResponse BodyReader)
clientCommon man = GenericClient
    { clientStatus = responseStatus . hrFinalResponse
    , clientHost = T.pack . B.unpack . host . hrFinalRequest
    , clientRefer = \resp (path, query) -> acquireResponse man $ hrFinalRequest resp
        & _path .~ toStricByteString (encodePathSegments path)
        & _queryString .~ toStricByteString (renderQueryText True query)
        & _requestHeaders %~ setHeader hReferer (asReferer resp)
        & _cookieJar .~ Just (responseCookieJar (hrFinalResponse resp))
    }


data HistoriedCloudflareChallengeException = HistoriedCloudflareChallengeException
    { cloudflareHistory :: [HistoriedResponse L.ByteString]
    , cloudflareException :: CloudflareChallengeException (HistoriedResponse L.ByteString)
    } deriving Show

instance Exception HistoriedCloudflareChallengeException


throughChallenge :: (MonadThrow m, MonadResource m)
                 => Maybe Int
                 -> Maybe Int
                 -> Manager
                 -> Request
                 -> m (Either CloudflareChallengeFlowException (ReleaseKey, HistoriedResponse BodyReader))
throughChallenge maxChallenges maxBodySize man req = do
    r <- runExceptT $ CF.throughChallenge
        (client maxBodySize man)
        maxChallenges
        (acquireResponse man (ensureCookieJar req))
    case r of
        Left (CloudflareChallengeFlowException ex (key, _)) -> Left ex <$ release key
        Left (CloudflareChallengePageException ex) -> throwM ex
        Right resp -> return $ Right resp

throughChallengeWithLogging :: MonadResource m
                            => Maybe Int
                            -> Maybe Int
                            -> Manager
                            -> Request
                            -> m (Either HistoriedCloudflareChallengeException (ReleaseKey, HistoriedResponse BodyReader))
throughChallengeWithLogging maxChallenges maxBodySize man req = do
    (r, hist) <- runWriterT . runExceptT $ CF.throughChallenge
        (clientWithLogging (tell . (:[])) maxBodySize man)
        maxChallenges
        (acquireResponse man (ensureCookieJar req))
    case r of
        Left ex -> Left . HistoriedCloudflareChallengeException hist <$> freeze ex
        Right resp -> return $ Right resp
  where
    freeze (CloudflareChallengeFlowException ex (key, resp)) = do
        resp' <- readBody maxBodySize resp
        release key
        return $ CloudflareChallengeFlowException ex resp'
    freeze (CloudflareChallengePageException ex) = return (CloudflareChallengePageException ex)

withThroughChallenge :: (MonadUnliftIO m, MonadThrow m)
                     => Maybe Int
                     -> Maybe Int
                     -> Manager
                     -> Request
                     -> (HistoriedResponse BodyReader -> m a)
                     -> m (Either CloudflareChallengeFlowException a)
withThroughChallenge maxChallenges maxBodySize man req f = runResourceT $ do
    r <- throughChallenge maxChallenges maxBodySize man req
    case r of
        Left ex -> return $ Left ex
        Right (_, resp) -> lift $ Right <$> f resp

withThroughChallengeWithLogging :: (MonadUnliftIO m, MonadThrow m)
                                => Maybe Int
                                -> Maybe Int
                                -> Manager
                                -> Request
                                -> (HistoriedResponse BodyReader -> m a)
                                -> m (Either HistoriedCloudflareChallengeException a)
withThroughChallengeWithLogging maxChallenges maxBodySize man req f = runResourceT $ do
    r <- throughChallengeWithLogging maxChallenges maxBodySize man req
    case r of
        Left ex -> return $ Left ex
        Right (_, resp) -> lift $ Right <$> f resp

withThroughChallengeSimple :: (MonadUnliftIO m, MonadThrow m)
                           => Manager
                           -> Request
                           -> (Response BodyReader -> m a)
                           -> m a
withThroughChallengeSimple man req f = either throwM return
    =<< withThroughChallenge defaultMaxChallenges defaultMaxBodySize man req (f . hrFinalResponse)

withThroughChallengeWithLoggingSimple :: (MonadUnliftIO m, MonadThrow m)
                                      => Manager
                                      -> Request
                                      -> (Response BodyReader -> m a)
                                      -> m a
withThroughChallengeWithLoggingSimple man req f = either throwM return
    =<< withThroughChallengeWithLogging defaultMaxChallenges defaultMaxBodySize man req (f . hrFinalResponse)


defaultMaxChallenges :: Maybe Int
defaultMaxChallenges = Just 5

defaultMaxBodySize :: Maybe Int
defaultMaxBodySize = Just 65536

acquireResponse :: Manager -> Request -> Acquire (HistoriedResponse BodyReader)
acquireResponse man req = mkAcquire (responseOpenHistory req man) (responseClose . hrFinalResponse)

readBody :: MonadIO m => Maybe Int -> HistoriedResponse BodyReader -> m (HistoriedResponse L.ByteString)
readBody maxBodySize = _hrFinalResponse . _responseBody %%~
    (flip connect) (maybe id (fuse . isolate) maxBodySize sinkLazy) . bodyReaderSource

setHeader :: HeaderName -> B.ByteString -> [Header] -> [Header]
setHeader h v = (:) (h, v) . filter ((/=) h . fst)

asReferer :: HistoriedResponse body -> B.ByteString
asReferer = requestUrl . hrFinalRequest

requestUrl :: Request -> B.ByteString
requestUrl = scheme <> host <> (port_ <$> secure <*> port) <> path <> queryString
  where
    scheme = bool "https://" "http://" <$> secure
    port_ True 443 = ""
    port_ False 80 = ""
    port_ _ oth = ":" <> B.pack (show oth)

ensureCookieJar :: Request -> Request
ensureCookieJar = _cookieJar %~ Just . fromMaybe (createCookieJar [])

toStricByteString :: Builder -> B.ByteString
toStricByteString = L.toStrict . toLazyByteString
