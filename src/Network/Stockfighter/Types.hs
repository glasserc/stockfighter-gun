module Network.Stockfighter.Types (
    ApiKey,
    -- FIXME: maybe don't export the constructor
    StockfighterEnvironment(..),
    ) where

import Data.ByteString (ByteString)
import qualified Network.Wreq.Session as S

type ApiKey = ByteString

-- | A handle on a Stockfighter instance.
data StockfighterEnvironment = SE {
    apiKey :: ApiKey,
    session :: S.Session
    }
