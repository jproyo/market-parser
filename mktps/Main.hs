module Main where

import qualified Data.ByteString.Lazy as BL
import           MarketParser.Parser  (ParseStrategy (..), getQuotes)
import           System.Environment

main :: IO ()
main = do
  orderStrategy <- getArgs >>= parse
  contents      <- BL.getContents
  print (getQuotes contents orderStrategy)

parse :: [String] -> IO ParseStrategy
parse ["-r"] = return SortByAcceptTime
parse _      = putStrLn "Usage: -r to sort by accept time quote" >> return Unsorted
