module MarketParser (parseMarketData) where

import qualified Data.ByteString.Lazy    as BL (ByteString)
import           MarketParser.Parser     (parse)
import           MarketParser.QuoteModel (Quotes)

parseMarketData :: BL.ByteString -> Quotes
parseMarketData = parse
