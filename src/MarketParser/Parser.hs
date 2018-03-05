{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module MarketParser.Parser (getQuotes) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (guard, replicateM)
import           Control.Monad.Loops        (untilM)
import           Data.Binary.Get            (Get, getByteString, getWord32le,
                                             isEmpty, label, runGet, skip)
import           Data.ByteString.Conversion as Conv (fromByteString)
import qualified Data.ByteString.Lazy       as BL (ByteString)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T (Text, unpack)
import qualified Data.Text.Encoding         as Enc (decodeUtf8)
import           Data.UnixTime              (UnixTime (..))
import           MarketParser.QuoteModel


isValidPacketSize :: Int -> Bool
isValidPacketSize size = size == 215

isValidQuote :: T.Text -> Bool
isValidQuote mark = mark == "B6034"

getQuotes :: BL.ByteString -> Maybe Quotes
getQuotes str = runGet allQuotes str

allQuotes :: Get (Maybe Quotes)
allQuotes = pcapHeader >> Just <$> (untilM getQuote isEmpty) <|> pure Nothing

pcapHeader :: Get ()
pcapHeader = skip 24

skipAndGetQuote :: Int -> Get Quote
skipAndGetQuote pktSize = skip pktSize >> getQuote

getQuote :: Get Quote
getQuote = label "GET QUOTE" $ do
  pktTimeV    <- packetTime
  packetSizeV <- packetSize
  (guard (isValidPacketSize packetSizeV) >> continueParsing pktTimeV) <|> skipAndGetQuote packetSizeV
  where continueParsing pktTimeV  = do
          dataInfoMarketType <- Enc.decodeUtf8 <$> getByteString 5
          (guard (isValidQuote dataInfoMarketType) >> quote pktTimeV dataInfoMarketType) <|> skipAndGetQuote 210

intNumber :: Get Integer
intNumber = toInteger <$> getWord32le

packetSize :: Get Int
packetSize = label "GET PACKET SIZE" $ do
  skip 4
  paket <- (abs . ((-) 42) . fromInteger) <$> intNumber
  skip 42
  return paket

packetTime :: Get UnixTime
packetTime = label "GET PACKET TIME" $ do
  utSeconds      <- fromInteger <$> intNumber
  utMicroSeconds <- fromInteger <$> intNumber
  return UnixTime {..}

bestPriceQuantity :: Get QuoteBstPrcQty
bestPriceQuantity = do
  bestPrice    <- (fromJust . Conv.fromByteString) <$> getByteString 5
  bestQuantity <- (fromJust . Conv.fromByteString) <$> getByteString 7
  return QuoteBstPrcQty {..}

bestPriceQuantities :: Get [QuoteBstPrcQty]
bestPriceQuantities = replicateM 5 bestPriceQuantity

totalVol :: Get Int
totalVol = (fromJust . Conv.fromByteString) <$> getByteString 7

quoteBest :: Get QuoteBest
quoteBest = do
  totalValid <- (fromJust . Conv.fromByteString) <$> getByteString 5
  bestQuote  <- replicateM 5 $ (fromJust . Conv.fromByteString) <$> getByteString 4
  return QuoteBest {..}

issueCodeP :: Get String
issueCodeP = (T.unpack . Enc.decodeUtf8) <$> getByteString 12

issueSeqP :: Get Int
issueSeqP = (fromJust . Conv.fromByteString) <$> getByteString 3

marketStatusTypeP :: Get String
marketStatusTypeP = (T.unpack . Enc.decodeUtf8) <$> getByteString 2

acceptTimeP :: Get String
acceptTimeP = (T.unpack . Enc.decodeUtf8) <$> getByteString 8

quote :: UnixTime -> T.Text -> Get Quote
quote pktTime dataInfoMarketType = do
  let marketType   = [last . T.unpack $ dataInfoMarketType]
  issueCode        <- issueCodeP
  issueSeq         <- issueSeqP
  marketStatusType <- marketStatusTypeP
  bidDetail        <- QuoteBestBid <$> totalVol <*> bestPriceQuantities
  askDetail        <- QuoteBestAsk <$> totalVol <*> bestPriceQuantities
  bestBid          <- quoteBest
  bestAsk          <- quoteBest
  acceptTime       <- acceptTimeP
  skip 1
  return $ Quote {..}
