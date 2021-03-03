module Main where

import RIO

import Data.Yaml (decodeFileEither)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv, parseBaseUrl)
import System.Environment (getEnv)

import Lib


main :: IO ()
main = do
  home <- getEnv "HOME"
  Right config <- decodeFileEither @[Config] (home <> "/.cloudflare-ddns.yaml")

  manager <- newTlsManager
  root <- parseBaseUrl "https://api.cloudflare.com/client/v4/"

  for_ config (update (mkClientEnv manager root))
