{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Stockfighter.Requests (
    heartbeat,
    ) where

import Control.Lens ((&), (.~), (^.))
import Network.Stockfighter.Responses (HeartbeatResponse)
import Network.Stockfighter.Types (StockfighterEnvironment(SE, apiKey, session))
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
