module MarketParser.QuoteModel where

import           Data.Monoid   ((<>))
import           Data.UnixTime (UnixTime (..))
import           Data.UnixTime (formatUnixTimeGMT, webDateFormat)

data QuoteBstPrcQty = QuoteBstPrcQty
  { bestPrice    :: Double,
    bestQuantity :: Double
  }

data QuoteBestBid = QuoteBestBid
  { totalBidVol :: Int
  , priceBidQty :: [QuoteBstPrcQty] }

data QuoteBestAsk = QuoteBestAsk
  { totalAskVol :: Int
  , priceAskQty :: [QuoteBstPrcQty] }

data QuoteBest = QuoteBest
  { totalValid :: Int,
    bestQuote  :: [Int]
  }

data Quote = Quote
  { pktTime          :: UnixTime,
    acceptTime       :: String,
    issueCode        :: String,
    issueSeq         :: Int,
    marketType       :: String,
    marketStatusType :: String,
    bidDetail        :: QuoteBestBid,
    askDetail        :: QuoteBestAsk,
    bestBid          :: QuoteBest,
    bestAsk          :: QuoteBest
  }

type Quotes = [Quote]

instance Show Quote where
  show q = "<quote>"
        <> "<pkt-time>" <> (show $ formatUnixTimeGMT webDateFormat (pktTime q)) <> "</pkt-time>"
        <> "<accept-time>" <> (acceptTime q) <> "</accept-time>"
        <> "<issue-code>" <> (issueCode q) <> "</issue-code>"
        <> (show $ bidDetail q)
        <> "<asks>" <> (show $ askDetail q) <> "</asks>"
        <> "</quote>"

showPrice :: String -> (Int, QuoteBstPrcQty) -> String
showPrice mark (nro, prcQty) = "<" <> mark <> "qty" <> (show nro) <> ">" <> (show $ bestQuantity prcQty) <> "</" <> mark <> "qty" <> (show nro) <> ">"
                            <> "<" <> mark <> "price" <> (show nro) <> ">" <> (show $ bestPrice prcQty) <> "</" <> mark <> "price" <> (show nro) <> ">"

instance Show QuoteBestBid where
  show bid = "<bids>" <> (concat $ map (showPrice "b") (reverse $ zip [1..5] (priceBidQty bid))) <> "</bids>"

instance Show QuoteBestAsk where
  show ask = "<asks>" <> (concat $ map (showPrice "a") (zip [1..5] (priceAskQty ask))) <> "</asks>"
