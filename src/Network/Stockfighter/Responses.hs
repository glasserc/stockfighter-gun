{-# LANGUAGE OverloadedStrings #-}
module Network.Stockfighter.Responses (
    HeartbeatResponse(..),
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(parseJSON), (.:), withObject)
import Data.Text (Text)

data HeartbeatResponse = HeartbeatResponse {
    heartbeatOK :: Bool,
    heartbeatError :: Text
    } deriving Show

instance FromJSON HeartbeatResponse where
    parseJSON = withObject "HeartbeatResponse" $ \o ->
        HeartbeatResponse <$> o .: "ok" <*> o .: "error"
