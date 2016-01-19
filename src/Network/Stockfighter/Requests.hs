{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Stockfighter.Requests (
    heartbeat,
    newOrder
    ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.Text as T
import Network.Stockfighter.Responses (HeartbeatResponse)
import Network.Stockfighter.Types (Order,
    RequestOrder(RequestOrder, roStock, roVenue),
    StockfighterEnvironment(SE, apiKey, session))
import Network.Wreq (Options, asJSON, defaults, header, responseBody)
import qualified Network.Wreq.Session as S

stockfighterOptions :: StockfighterEnvironment -> Options
stockfighterOptions SE {apiKey} =
    defaults & header "X-Starfighter-Auth" .~ [apiKey]

heartbeat :: StockfighterEnvironment -> IO HeartbeatResponse
heartbeat se@SE{session} = do
    let opts = stockfighterOptions se
    response <- S.getWith opts session "https://api.stockfighter.io/ob/api/heartbeat" >>= asJSON
    return $ response ^. responseBody

newOrder :: RequestOrder -> StockfighterEnvironment -> IO Order
newOrder o@RequestOrder{roVenue, roStock} se@SE {session} = do
    let opts = stockfighterOptions se
        url = concat [
            "https://api.stockfighter.io/ob/api/venues/",
            T.unpack roVenue,
            "/stocks/",
            T.unpack roStock,
            "/orders"
            ]
    response <- S.postWith opts session url (encode o) >>= asJSON
    return $ response ^. responseBody
