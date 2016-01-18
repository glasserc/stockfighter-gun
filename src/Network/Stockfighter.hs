{-# LANGUAGE NamedFieldPuns #-}
module Network.Stockfighter (
    withStockfighterEnvironment,
    ) where

import Data.ByteString (ByteString)
import Network.Stockfighter.Types (StockfighterEnvironment(SE, apiKey, session))
import qualified Network.Wreq.Session as S

withStockfighterEnvironment :: ByteString -> (StockfighterEnvironment -> IO a) -> IO a
withStockfighterEnvironment apiKey f = S.withSession $ \session ->
    f SE {apiKey, session}
