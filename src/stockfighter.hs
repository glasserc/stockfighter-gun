{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.Aeson (FromJSON(parseJSON), (.:), withObject)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (hGetContents)
import Network.Stockfighter (withStockfighterEnvironment)
import Network.Stockfighter.Requests (heartbeat)
import Network.Stockfighter.Types (ApiKey)
import qualified Network.Wreq.Session as S
import System.Environment (getEnv)
import System.IO (IOMode(ReadMode), withFile)


main :: IO ()
main = do
    apiKey <- getApiKey
    response <- withStockfighterEnvironment apiKey heartbeat
    print response

getApiKey :: IO ApiKey
getApiKey = do
    home <- getEnv "HOME"
    encodeUtf8 <$> withFile (home ++ "/.stockfighter") ReadMode hGetContents
