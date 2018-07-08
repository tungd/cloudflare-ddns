module Lib where

import RIO


data Config = Config
  { email :: Text
  , apiKey :: Text
  , domain :: Text
  } deriving (Eq, Show, Generic)


config = Config
  { email = "me@tungdao.com"
  , apiKey = "ed3320f7ce5e08facb4b138778ad4b62960cb"
  , domain = "mbp.tungdao.com"
  }


someFunc :: IO ()
someFunc = undefined
