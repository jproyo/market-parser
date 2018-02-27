module Main where

import qualified Data.ByteString.Lazy as BL
import           MarketParser         (parseMarketData)

main :: IO ()
main = do
  contents <- BL.getContents
  putStrLn $ show (parseMarketData contents)
