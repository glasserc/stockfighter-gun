{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Stockfighter.Requests (
    cancelOrder,
    heartbeat,
    getOrderStatus,
    getQuote,
    newOrder
    ) where

import Control.Exception (throwIO)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.Text as T
import Network.Stockfighter.Types (Envelope(ESuccess, EFailure),
    HeartbeatResponse,
    Order, OrderId, Quote, Stock, Venue,
    RequestOrder(RequestOrder, roStock, roVenue),
    StockfighterEnvironment(SE, apiKey, session),
    StockfighterResponseException(StockfighterResponseException))
import Network.Wreq (Options, asJSON, defaults, header, responseBody)
import qualified Network.Wreq.Session as S

stockfighterOptions :: StockfighterEnvironment -> Options
stockfighterOptions SE {apiKey} =
    defaults & header "X-Starfighter-Authorization" .~ [apiKey]

heartbeat :: StockfighterEnvironment -> IO HeartbeatResponse
heartbeat se@SE{session} = do
    let opts = stockfighterOptions se
    response <- S.getWith opts session "https://api.stockfighter.io/ob/api/heartbeat" >>= asJSON
    return $ response ^. responseBody

newOrder :: RequestOrder -> StockfighterEnvironment -> IO Order
newOrder o@RequestOrder{roVenue, roStock} se@SE {session} = do
    let opts = stockfighterOptions se
        url = stockURL roVenue roStock ++ "/orders"

    response <- S.postWith opts session url (encode o) >>= asJSON
    checkEnvelope $ response ^. responseBody

getQuote :: Venue -> Stock -> StockfighterEnvironment -> IO Quote
getQuote venue stock se@SE {session} = do
    let opts = stockfighterOptions se
        url = stockURL venue stock ++ "/quote"

    response <- S.getWith opts session url >>= asJSON
    checkEnvelope $ response ^. responseBody

getOrderStatus :: Venue -> Stock -> OrderId -> StockfighterEnvironment -> IO Order
getOrderStatus venue stock oId se@SE {session} = do
    let opts = stockfighterOptions se
        url = stockURL venue stock ++ "/orders/" ++ show oId

    response <- S.getWith opts session url >>= asJSON
    checkEnvelope $ response ^. responseBody

cancelOrder :: Venue -> Stock -> OrderId -> StockfighterEnvironment -> IO Order
cancelOrder venue stock oId se@SE {session} = do
    let opts = stockfighterOptions se
        url = stockURL venue stock ++ "/orders/" ++ show oId

    response <- S.deleteWith opts session url >>= asJSON
    checkEnvelope $ response ^. responseBody

-- | Base URL for requests affecting a stock on some venue.
stockURL :: Venue -> Stock -> String
stockURL venue stock = concat [
    "https://api.stockfighter.io/ob/api/venues/",
    T.unpack venue,
    "/stocks/",
    T.unpack stock
    ]


-- | Check that an envelope wraps a successful response, or throw an error.
checkEnvelope :: Envelope a -> IO a
checkEnvelope (ESuccess a) = return a
checkEnvelope (EFailure s) = throwIO $ StockfighterResponseException s
