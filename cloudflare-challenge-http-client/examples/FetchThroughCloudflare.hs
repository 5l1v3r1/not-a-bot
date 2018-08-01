{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad ((>=>))
import Data.Conduit (connect)
import Data.Conduit.Combinators (stdout)
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.Lens
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header
import System.Environment (getArgs)

import Network.HTTP.Cloudflare.Challenge

main :: IO ()
main = do
    [url] <- getArgs
    fetchThroughCloudflare url

-- Fetch a URL, solving Cloudflare challenges if they are presented, and write the response to stdout
fetchThroughCloudflare :: String -> IO ()
fetchThroughCloudflare url = do
    man <- newManager $ tlsManagerSettings & _managerModifyRequest %~ (<&>) (& _requestHeaders <>~ extraHeaders)
    let exec = flip responseOpenHistory man
    req <- parseRequest url
    withFaceCloudflareChallenge_ exec req $ \resp ->
        bodyReaderSource (responseBody resp) `connect` stdout

-- Reduce the likelyhood of Cloudflare responding with a CAPTCHA
extraHeaders :: [Header]
extraHeaders =
    [ (hAccept, "*/*")
    , (hAcceptLanguage, "en-US,en;q=0.9")
    , (hUserAgent, userAgent)
    ]

userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36"
