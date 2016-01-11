{-# LANGUAGE OverloadedStrings #-}
import Control.Lens ((&), (.~))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (hGetContents)
import Network.Wreq (defaults, getWith, header)
import System.Environment (getEnv)
import System.IO (IOMode(ReadMode), withFile)

type ApiKey = Text

main :: IO ()
main = do
    apiKey <- getApiKey
    let opts = defaults & header "X-Starfighter-Auth" .~ [encodeUtf8 apiKey]
    response <- getWith opts "https://api.stockfighter.io/ob/api/heartbeat"
    print response

getApiKey :: IO ApiKey
getApiKey = do
    home <- getEnv "HOME"
    withFile (home ++ "/.stockfighter") ReadMode hGetContents
