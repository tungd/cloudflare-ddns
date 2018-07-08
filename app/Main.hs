module Main where

import Lib
import Network.HTTP.Client.TLS
import RIO
import Servant.Client

config :: Config
config = Config
  { email = "me@tungdao.com"
  , apiKey = "ed3320f7ce5e08facb4b138778ad4b62960cb"
  , domain = "mbp.tungdao.com"
  }

main :: IO ()
main = do
  manager <- newTlsManager
  baseUrl <- parseBaseUrl "https://api.cloudflare.com/client/v4/"
  -- TODO: read in config from yaml file
  update (mkClientEnv manager baseUrl) config
