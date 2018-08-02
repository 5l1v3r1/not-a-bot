{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Cloudflare.Challenge

import Control.Exception (throwIO)
import Control.Monad
import Network.HTTP.Types
import qualified Data.Text as T
import System.Exit
import System.IO
import Text.HTML.DOM as HTML

main :: IO ()
main = do
    sol <- solveFile "tests/challenge-page.html"
    unless (sol == (path, timeout)) $ do
        hPrint stderr sol
        exitFailure

solveFile :: FilePath -> IO (([T.Text], QueryText), Int)
solveFile fp = do
    doc <- HTML.readFile fp
    r <- solveChallenge domain doc
    case r of
        Left ex -> throwIO (CloudflareChallengePageException ex :: CloudflareChallengeException ())
        Right sol -> return sol

domain :: T.Text
domain = "nickspinale.com"

path =
    ( ["cdn-cgi","l","chk_jschl"]
    , [ ("jschl_vc", Just "078f40be5103f0b2aae87b85f5ce53e4")
      , ("pass", Just "1533035574.481-vGNp3pTcJ5")
      , ("jschl_answer", Just "19.602207111200002")
      ]
    )

timeout = 4000
