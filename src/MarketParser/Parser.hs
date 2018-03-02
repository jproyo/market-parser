{-# LANGUAGE RecordWildCards #-}

module MarketParser.Parser (parse) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (guard, replicateM)
import           Control.Monad.Loops        (untilM)
import           Data.Binary.Get            (Get, getByteString, getWord32le,
                                             isEmpty, runGet, skip)
import           Data.ByteString.Conversion as Conv (fromByteString)
import qualified Data.ByteString.Lazy       as BL (ByteString)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T (Text, unpack)
import qualified Data.Text.Encoding         as Enc (decodeUtf8)
import           Data.UnixTime              (UnixTime (..))
import           MarketParser.QuoteModel


validPacketSize :: Int
validPacketSize = 215

validASCIIQuote :: T.Text
validASCIIQuote = "B6034"

parse :: BL.ByteString -> Quotes
parse = runGet parseAll

parseAll :: Get Quotes
parseAll = pcapHeader >> Quotes <$> untilM parseData isEmpty

pcapHeader :: Get ()
pcapHeader = skip 24

parseData :: Get Quote
parseData = do
  pktTimeV   <- parsePktTime
  packetSize <- parsePaketSize
  (guard (packetSize == validPacketSize) >> continueParsing pktTimeV) <|> skipAndParse packetSize
  where skipAndParse    bytes     = skip bytes >> parseData
        continueParsing pktTimeV  = do
          dataInfoMarketType <- Enc.decodeUtf8 <$> getByteString 5
          (guard (dataInfoMarketType == validASCIIQuote) >> parseQuote pktTimeV dataInfoMarketType) <|> skipAndParse 210

parsePaketSize :: Get Int
parsePaketSize = do
  skip 4
  paket <- (abs . ((-) 42) . fromInteger . toInteger) <$> getWord32le
  skip 42
  return paket

parsePktTime :: Get UnixTime
parsePktTime = do
  utSeconds      <- (fromIntegral . toInteger) <$> getWord32le
  utMicroSeconds <- (fromIntegral . toInteger) <$> getWord32le
  return UnixTime {..}

parseBestPriceQuantity :: Get QuoteBstPrcQty
parseBestPriceQuantity = do
  bestPrice    <- (fromJust . Conv.fromByteString) <$> getByteString 5
  bestQuantity <- (fromJust . Conv.fromByteString) <$> getByteString 7
  return QuoteBstPrcQty {..}

parseBestPriceQuantities :: Get [QuoteBstPrcQty]
parseBestPriceQuantities = replicateM 5 parseBestPriceQuantity

parseQuoteDetail :: Get QuoteDetail
parseQuoteDetail = do
  totalQuoteVol <- (fromJust . Conv.fromByteString) <$> getByteString 7
  priceQty      <- parseBestPriceQuantities
  return QuoteDetail {..}

parseQuoteBest :: Get QuoteBest
parseQuoteBest = do
  totalValid <- (fromJust . Conv.fromByteString) <$> getByteString 5
  bestQuote  <- replicateM 5 $ (fromJust . Conv.fromByteString) <$> getByteString 4
  return QuoteBest {..}

parseQuote :: UnixTime -> T.Text -> Get Quote
parseQuote pktTime dataInfoMarketType = do
  let marketType   = [last . T.unpack $ dataInfoMarketType]
  issueCode        <- (T.unpack . Enc.decodeUtf8) <$> getByteString 12
  issueSeq         <- (fromJust . Conv.fromByteString) <$> getByteString 3
  marketStatusType <- (T.unpack . Enc.decodeUtf8) <$> getByteString 2
  bidDetail        <- parseQuoteDetail
  askDetail        <- parseQuoteDetail
  bestBid          <- parseQuoteBest
  bestAsk          <- parseQuoteBest
  acceptTime       <- (T.unpack . Enc.decodeUtf8) <$> getByteString 8
  skip 1
  return $ Quote {..}
