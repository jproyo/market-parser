{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MarketParser.Parser (ParseStrategy(..), getQuotes) where

import           Control.Applicative           ((<|>))
import           Control.Monad                 (guard, replicateM)
import           Data.Binary.Get               (Decoder (..), Get,
                                                getByteString, getWord32le,
                                                label, lookAheadM,
                                                runGetIncremental, skip)
import qualified Data.ByteString               as BS (ByteString, null)
import           Data.ByteString.Builder       (toLazyByteString, word32Hex)
import           Data.ByteString.Conversion    as Conv (fromByteString)
import qualified Data.ByteString.Lazy          as BL (ByteString, empty)
import qualified Data.ByteString.Lazy.Char8    as BLC (pack)
import qualified Data.ByteString.Lazy.Internal as BLI (ByteString (Chunk),
                                                       chunk)
import           Data.Fixed                    (Fixed (MkFixed))
import           Data.List                     (insert)
import           Data.Maybe                    (fromJust, fromMaybe, isJust)
import qualified Data.Text                     as T (Text, unpack)
import qualified Data.Text.Encoding            as Enc (decodeUtf8)
import           Data.Time.LocalTime           (TimeOfDay, makeTimeOfDayValid,
                                                midnight)
import           Data.UnixTime                 (UnixTime (..))
import           MarketParser.QuoteModel

data ParseStrategy = SortByAcceptTime
                   | Unsorted


insertWith :: Ord a => ParseStrategy -> a -> [a] -> [a]
insertWith SortByAcceptTime = insert
insertWith Unsorted         = (:)

isValidPacketSize :: Int -> Bool
isValidPacketSize size = size == 215

isValidQuote :: T.Text -> Bool
isValidQuote mark = mark == "B6034"

getQuotes :: BL.ByteString -> ParseStrategy -> Either String Quotes
getQuotes = decodeQuote decoder

decodeQuote :: Decoder Quote -> BL.ByteString -> ParseStrategy -> Either String Quotes
decodeQuote (Done leftover _consumed quoteDec) input strategy =
  if BS.null leftover then Right [quoteDec]
  else insertWith strategy quoteDec <$> decodeQuote decoder (BLI.chunk leftover input) strategy
decodeQuote (Partial k) input strategy                        =
  decodeQuote (k . takeHeadChunk $ input) (dropHeadChunk input) strategy
decodeQuote (Fail _leftover _consumed msg) _ _                =
  Left msg

decoder :: Decoder Quote
decoder = runGetIncremental headerOrQuote

takeHeadChunk :: BL.ByteString -> Maybe BS.ByteString
takeHeadChunk (BLI.Chunk bs _) = Just bs
takeHeadChunk _                = Nothing

dropHeadChunk :: BL.ByteString -> BL.ByteString
dropHeadChunk (BLI.Chunk _ lbs) = lbs
dropHeadChunk _                 = BL.empty

headerOrQuote :: Get Quote
headerOrQuote = do
  attemp <- lookAheadM pcapHeader
  (guard (isJust attemp) >> skip 20 *> getQuote) <|> getQuote


pcapHeader :: Get (Maybe BL.ByteString)
pcapHeader = do
  magic <- (toLazyByteString . word32Hex) <$> getWord32le
  if magic == BLC.pack "a1b2c3d4" then return (Just magic) else return Nothing

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
  paket <- (abs . (42 -) . fromInteger) <$> intNumber
  skip 42
  return paket

packetTime :: Get UnixTime
packetTime = label "GET PACKET TIME" $ do
  utSeconds      <- fromInteger <$> intNumber
  utMicroSeconds <- fromInteger <$> intNumber
  return UnixTime {..}

bestPriceQuantity :: Get QuoteBstPrcQty
bestPriceQuantity = label "GET BEST PRICE QUANTITY" $ do
  bestPrice    <- (fromJust . Conv.fromByteString) <$> getByteString 5
  bestQuantity <- (fromJust . Conv.fromByteString) <$> getByteString 7
  return QuoteBstPrcQty {..}

bestPriceQuantities :: Get [QuoteBstPrcQty]
bestPriceQuantities = replicateM 5 bestPriceQuantity

totalVol :: Get Int
totalVol = (fromJust . Conv.fromByteString) <$> getByteString 7

quoteBest :: Get QuoteBest
quoteBest = label "GET BEST QUOTE" $ do
  totalValid <- (fromJust . Conv.fromByteString) <$> getByteString 5
  bestQuote  <- replicateM 5 $ (fromJust . Conv.fromByteString) <$> getByteString 4
  return QuoteBest {..}

issueCodeP :: Get String
issueCodeP = (T.unpack . Enc.decodeUtf8) <$> getByteString 12

issueSeqP :: Get Int
issueSeqP = (fromJust . Conv.fromByteString) <$> getByteString 3

marketStatusTypeP :: Get String
marketStatusTypeP = (T.unpack . Enc.decodeUtf8) <$> getByteString 2

acceptTimeP :: Get TimeOfDay
acceptTimeP = label "GET ACCEPT TIME" $  do
  time <- (T.unpack . Enc.decodeUtf8) <$> getByteString 8
  let hour = read . take 2
  let minute = read . take 2 . drop 2
  let sec = MkFixed . read . take 2 . drop 4
  return $ fromMaybe midnight (makeTimeOfDayValid (hour time) (minute time) (sec time))

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
  return Quote {..}
