{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Stockfighter.Requests (
    heartbeat,
    getQuote,
    newOrder
    ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.Text as T
import Network.Stockfighter.Responses (HeartbeatResponse)
import Network.Stockfighter.Types (Order, Quote, Stock, Venue,
    RequestOrder(RequestOrder, roStock, roVenue),
    StockfighterEnvironment(SE, apiKey, session))
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
    return $ response ^. responseBody

getQuote :: Venue -> Stock -> StockfighterEnvironment -> IO Quote
getQuote venue stock se@SE {session} = do
    let opts = stockfighterOptions se
        url = stockURL venue stock ++ "/quote"

    response <- S.getWith opts session url >>= asJSON
    return $ response ^. responseBody

-- | Base URL for requests affecting a stock on some venue.
stockURL :: Venue -> Stock -> String
stockURL venue stock = concat [
    "https://api.stockfighter.io/ob/api/venues/",
    T.unpack venue,
    "/stocks/",
    T.unpack stock
    ]
