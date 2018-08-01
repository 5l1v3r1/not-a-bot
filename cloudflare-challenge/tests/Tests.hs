{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Cloudflare.Challenge

import System.Exit
import System.IO
import Text.HTML.DOM as HTML

main :: IO ()
main = do
    doc <- HTML.readFile "tests/challenge-page.html"
    r <- solveChallenge doc (length domain)
    case r of
        Left ex -> do
            hPrint stderr ex
            exitFailure
        Right ((p, q), t) | p == path && q == query && t == timeout -> do
            exitSuccess
        Right wrong -> do
            hPrint stderr wrong
            exitFailure

domain :: String
domain = "nickspinale.com"

path = "/cdn-cgi/l/chk_jschl"
query = "?jschl_vc=078f40be5103f0b2aae87b85f5ce53e4&pass=1533035574.481-vGNp3pTcJ5&jschl_answer=19.602207111200002"
timeout = 4000
