module Main where

import Data.Yaml
import Network.HTTP.Client.TLS
import RIO
import Servant.Client
import System.Environment

import Lib

main :: IO ()
main = do
  manager <- newTlsManager
  baseUrl <- parseBaseUrl "https://api.cloudflare.com/client/v4/"
  home <- getEnv "HOME"
  Right config <- decodeFileEither (home <> "/.cloudflare-ddns.yaml")
  update (mkClientEnv manager baseUrl) config
