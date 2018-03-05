module MarketParser (parseMarketData) where

import qualified Data.ByteString.Lazy    as BL (ByteString)
import           MarketParser.Parser     (getQuotes)
import           MarketParser.QuoteModel (Quotes)

parseMarketData :: BL.ByteString -> Maybe Quotes
parseMarketData = getQuotes
