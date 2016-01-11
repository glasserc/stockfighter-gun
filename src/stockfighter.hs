{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (FromJSON(parseJSON), (.:), withObject)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (hGetContents)
import Network.Wreq (Options, asJSON, defaults, header, responseBody)
import qualified Network.Wreq.Session as S
import System.Environment (getEnv)
import System.IO (IOMode(ReadMode), withFile)

type ApiKey = ByteString

-- | A handle on a Stockfighter instance.
data StockfighterEnvironment = SE {
    apiKey :: ApiKey,
    session :: S.Session
    }

withStockfighterEnvironment :: ByteString -> (StockfighterEnvironment -> IO a) -> IO a
withStockfighterEnvironment apiKey f = S.withSession $ \session ->
    f SE {apiKey, session}

stockfighterOptions :: StockfighterEnvironment -> Options
stockfighterOptions SE {apiKey} =
    defaults & header "X-Starfighter-Auth" .~ [apiKey]

data HeartbeatResponse = HeartbeatResponse {
    heartbeatOK :: Bool,
    heartbeatError :: Text
    } deriving Show

instance FromJSON HeartbeatResponse where
    parseJSON = withObject "HeartbeatResponse" $ \o ->
        HeartbeatResponse <$> o .: "ok" <*> o .: "error"

heartbeat :: StockfighterEnvironment -> IO HeartbeatResponse
heartbeat se@SE{session} = do
    let opts = stockfighterOptions se
    response <- S.getWith opts session "https://api.stockfighter.io/ob/api/heartbeat" >>= asJSON
    return $ response ^. responseBody


main :: IO ()
main = do
    apiKey <- getApiKey
    response <- withStockfighterEnvironment apiKey heartbeat
    print response

getApiKey :: IO ApiKey
getApiKey = do
    home <- getEnv "HOME"
    encodeUtf8 <$> withFile (home ++ "/.stockfighter") ReadMode hGetContents
