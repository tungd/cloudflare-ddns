module Main where

import Data.Yaml
import Network.HTTP.Client.TLS
import RIO
import Servant.Client (mkClientEnv, parseBaseUrl)
import System.Environment

import Lib

main :: IO ()
main = do
  manager <- newTlsManager
  root <- parseBaseUrl "https://api.cloudflare.com/client/v4/"
  home <- getEnv "HOME"
  Right config <- decodeFileEither @[Config] (home <> "/.cloudflare-ddns.yaml")
  for_ config (update (mkClientEnv manager root))
