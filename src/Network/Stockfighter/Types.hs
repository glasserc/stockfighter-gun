{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.Stockfighter.Types (
    ApiKey,
    Direction(..),
    Envelope(..),
    Order(..),
    OrderType(..),
    Quote(..),
    RequestOrder(..),
    -- FIXME: maybe don't export the constructor
    StockfighterEnvironment(..),
    StockfighterResponseException(..),
    Stock,
    Venue,
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON),
                   Value(Object, String),
                   (.=), (.:), (.:?), object, withObject)
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.Typeable (Typeable)
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
    oStock :: Stock,
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
    parseJSON = withObject "Order" $ \o ->
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


data Quote = Quote {
    qStock :: Stock,
    qVenue :: Text,
    qBid :: Maybe Money,
    qAsk :: Maybe Money,
    qBidSize :: Int,
    qAskSize :: Int,
    qBidDepth :: Int,
    qAskDepth :: Int,
    qLastPrice :: Money,
    qLastQuantity :: Int,
    qLastTimestamp :: UTCTime, -- ^ Timestamp of last trade
    qTimestamp :: UTCTime  -- ^ Timestamp of last quote update
    } deriving Show

instance FromJSON Quote where
    parseJSON = withObject "Quote" $ \o ->
        Quote <$> o .: "symbol"
              <*> o .: "venue"
              <*> o .:? "bid"
              <*> o .:? "ask"
              <*> o .: "bidSize"
              <*> o .: "askSize"
              <*> o .: "bidDepth"
              <*> o .: "askDepth"
              <*> o .: "last"
              <*> o .: "lastSize"
              <*> o .: "lastTrade"
              <*> o .: "quoteTime"


data Envelope a = ESuccess a | EFailure Text
                deriving Show

instance FromJSON a => FromJSON (Envelope a) where
    parseJSON = withObject "Envelope" $ \o -> do
        isOK <- o .: "ok"
        if isOK
            then ESuccess <$> parseJSON (Object o)
            else do
            eError <- o .: "error"
            return $ EFailure eError


data StockfighterResponseException = StockfighterResponseException Text
    deriving (Typeable, Show)

instance Exception StockfighterResponseException
