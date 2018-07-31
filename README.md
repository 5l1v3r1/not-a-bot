Not a Bot
=========

Cloudflare 503 (not 403) challenge page bypass in Haskell.

More documentation coming soon.

## Example

The following program fetches a URL by solving Cloudflare JavaScript (503) challenges as they are presented, and writes the response to stdout. As one might expect, browsery headers seem to reduce the likelyhood of Cloudflare responding with a CAPTCHA (403) challenge.

```haskell
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

fetchThroughCloudflare :: String -> IO ()
fetchThroughCloudflare url = do
    man <- newManager $ tlsManagerSettings & _managerModifyRequest %~ (<&>) (& _requestHeaders <>~ extraHeaders)
    let exec = flip responseOpenHistory man
    req <- parseRequest url
    withFaceCloudflareChallenge_ exec req $ \resp ->
        bodyReaderSource (responseBody resp) `connect` stdout

extraHeaders :: [Header]
extraHeaders =
    [ (hAccept, "*/*")
    , (hAcceptLanguage, "en-US,en;q=0.9")
    , (hUserAgent, userAgent)
    ]

userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36"
```
