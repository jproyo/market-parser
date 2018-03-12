module MarketParser.QuoteModel where

import           Data.Monoid         ((<>))
import           Data.Time.LocalTime (TimeOfDay)
import           Data.UnixTime       (UnixTime (..), formatUnixTimeGMT,
                                      webDateFormat)

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
    acceptTime       :: TimeOfDay,
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

instance Eq Quote where
  (==) q1 q2 = issueCode q1 == issueCode q2
            && issueSeq q1 == issueSeq q2
            && marketType q1 == marketType q2
  (/=) q1 q2 = not $ (==) q1 q2

instance Ord Quote where
  compare q1 q2 = acceptTime q1 `compare` acceptTime q2
  (<=)    q1 q2 = acceptTime q1 <= acceptTime q2

instance Show Quote where
  show q = "<quote>"
        <> "<pkt-time>" <> show (formatUnixTimeGMT webDateFormat (pktTime q)) <> "</pkt-time>"
        <> "<accept-time>" <> show (acceptTime q) <> "</accept-time>"
        <> "<issue-code>" <> issueCode q <> "</issue-code>"
        <> show (bidDetail q)
        <> "<asks>" <> show (askDetail q) <> "</asks>"
        <> "</quote>"

showPrice :: String -> (Int, QuoteBstPrcQty) -> String
showPrice mark (nro, prcQty) =
  "<"
    <> mark
    <> "qty"
    <> show nro
    <> ">"
    <> show (bestQuantity prcQty)
    <> "</"
    <> mark
    <> "qty"
    <> show nro
    <> ">"
    <> "<"
    <> mark
    <> "price"
    <> show nro
    <> ">"
    <> show (bestPrice prcQty)
    <> "</"
    <> mark
    <> "price"
    <> show nro
    <> ">"

instance Show QuoteBestBid where
  show bid = "<bids>" <> concatMap (showPrice "b") (reverse $ zip [1..5] (priceBidQty bid)) <> "</bids>"

instance Show QuoteBestAsk where
  show ask = "<asks>" <> concatMap (showPrice "a") (zip [1..5] (priceAskQty ask)) <> "</asks>"
