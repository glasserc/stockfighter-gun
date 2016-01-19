{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Stockfighter.Types (
    ApiKey,
    Direction(..),
    Order(..),
    OrderType(..),
    RequestOrder(..),
    -- FIXME: maybe don't export the constructor
    StockfighterEnvironment(..),
    Stock,
    Venue,
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(String),
                   (.=), (.:), object, withObject)
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Network.Wreq.Session as S

type ApiKey = ByteString
-- | Stock ticker symbol representing a stock.
-- The API sometimes calls this a "stock" and sometimes a "symbol".
type Stock = Text
-- | Symbol representing a venue.
-- This is always called "venue".
type Venue = Text
type Money = Int  -- low 2 digits are cents

-- | A handle on a Stockfighter instance.
data StockfighterEnvironment = SE {
    apiKey :: ApiKey,
    session :: S.Session
    }

data RequestOrder = RequestOrder {
    roAccount :: Text,
    roVenue :: Venue,
    roStock :: Stock,
    roPrice :: Maybe Money,  -- can be omitted for Market orders
    roQuantity :: Int,
    roDirection :: Direction,
    roOrderType :: OrderType
    }

instance ToJSON RequestOrder where
    toJSON RequestOrder {..} = object [
        "account" .= roAccount,
        "venue" .= roVenue,
        "stock" .= roStock,
        "price" .= roPrice,
        "qty" .= roQuantity,
        "direction" .= roDirection,
        "orderType" .= roOrderType
        ]


data Direction =
    Sell | Buy
    deriving (Show, Read, Eq)

instance ToJSON Direction where
    toJSON Sell = String "sell"
    toJSON Buy = String "buy"

instance FromJSON Direction where
    parseJSON (String "sell") = return Sell
    parseJSON (String "buy") = return Buy
    parseJSON x = fail $ "couldn't parse Direction from " ++ show x


data OrderType =
    Limit |
    Market |
    FillOrKill |
    ImmediateOrCancel
    deriving (Show, Read, Eq)

instance ToJSON OrderType where
    toJSON Limit = String "limit"
    toJSON Market = String "market"
    toJSON FillOrKill = String "fill-or-kill"
    toJSON ImmediateOrCancel = String "immediate-or-cancel"

instance FromJSON OrderType where
    parseJSON (String "limit") = return Limit
    parseJSON (String "market") = return Market
    parseJSON (String "fill-or-kill") = return FillOrKill
    parseJSON (String "immediate-or-cancel") = return ImmediateOrCancel
    parseJSON x = fail $ "couldn't parse OrderType from " ++ show x


data Order = Order {
    oSymbol :: Stock,
    oVenue :: Venue,
    oDirection :: Direction,
    oOriginalQuantity :: Int,
    oQuantity :: Int,
    oPrice :: Money,
    oOrderType :: OrderType,
    oId :: Integer,
    oAccount :: Text,
    oTimestamp :: UTCTime,
    -- FIXME:
    --oFills
    oTotalFilled :: Int, -- presumably this matches oFills
    oOpen :: Bool
    } deriving (Show)

instance FromJSON Order where
    parseJSON = withObject "Order" $ \o -> do
        isOK <- o .: "ok"
        unless isOK $
            fail $ "parsing order failed -- source was " ++ show o
        Order <$> o .: "symbol"
              <*> o .: "venue"
              <*> o .: "direction"
              <*> o .: "originalQty"
              <*> o .: "qty"
              <*> o .: "price"
              <*> o .: "orderType"
              <*> o .: "id"
              <*> o .: "account"
              <*> o .: "ts"
              <*> o .: "totalFilled"
              <*> o .: "open"
