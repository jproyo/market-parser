module MarketParser.QuoteModel where

import           Data.UnixTime (UnixTime (..))

data QuoteBstPrcQty = QuoteBstPrcQty
  { bestPrice    :: Double,
    bestQuantity :: Double
  } deriving (Show)

data QuoteDetail = QuoteDetail
  { totalQuoteVol :: Int,
    priceQty      :: [QuoteBstPrcQty]
  } deriving (Show)

data QuoteBest = QuoteBest
  { totalValid :: Int,
    bestQuote  :: [Int]
  } deriving (Show)

data Quote = Quote
  { pktTime          :: UnixTime,
    acceptTime       :: String,
    issueCode        :: String,
    issueSeq         :: Int,
    marketType       :: String,
    marketStatusType :: String,
    bidDetail        :: QuoteDetail,
    askDetail        :: QuoteDetail,
    bestBid          :: QuoteBest,
    bestAsk          :: QuoteBest
  } deriving (Show)

newtype Quotes = Quotes [Quote]
  deriving (Show)
