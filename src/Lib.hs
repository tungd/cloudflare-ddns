module Lib where

import Data.Aeson hiding (Result)
import Network.Info
import RIO
import RIO.Char
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core

import qualified Data.Proxy as Proxy
import qualified RIO.Text as T


data Config = Config
  { email     :: !Text
  , apikey    :: !Text
  , domain    :: !Text
  , interface :: !String
  } deriving (Eq, Show, Generic)

instance FromJSON Config

newtype Result a = Result { result :: a }
  deriving (Show)

instance FromJSON a => FromJSON (Result a) where
  parseJSON = withObject "result" $ \o -> Result <$> o .: "result"

data Record = Record
  { recordId      :: !Text
  , recordName    :: !Text
  , recordContent :: !Text
  } deriving (Show, Generic)

instance ToJSON Record where
  toJSON Record{..} = object
    [ "type"    .= ("A" :: Text)
    , "proxied" .= False
    , "name"    .= recordName
    , "content" .= recordContent
    ]

instance FromJSON Record where
  parseJSON = genericParseJSON dropFieldLabelPrefix

data Zone = Zone { zoneId :: Text, zoneName :: Text }
  deriving (Show, Generic)

instance FromJSON Zone where
  parseJSON = genericParseJSON dropFieldLabelPrefix

data Credentials = Credentials
  deriving Typeable

instance (HasClient m api) => HasClient m (Credentials :> api) where
  type Client m (Credentials :> api) = Config -> Client m api

  clientWithRoute m _ req Config{..} =
    clientWithRoute m (Proxy.Proxy @api)
    $ addHeader "X-Auth-Email" email
    $ addHeader "X-Auth-Key" apikey req

  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy.Proxy @api) nt . cl

type ListZones = "zones"
  :> Credentials
  :> Get '[JSON] (Result [Zone])

type ListRecords = "zones"
  :> Credentials
  :> Capture "zone_uuid" Text
  :> "dns_records"
  :> Get '[JSON] (Result [Record])

type CreateRecord = "zones"
  :> Credentials
  :> Capture "zone_uuid" Text
  :> "dns_records"
  :> ReqBody '[JSON] Record
  :> Post '[JSON] (Result Record)

type UpdateRecord = "zones"
  :> Credentials
  :> Capture "zone_uuid" Text
  :> "dns_records"
  :> Capture "record_uuid" Text
  :> ReqBody '[JSON] Record
  :> Patch '[JSON] (Result Record)

type API = ListZones :<|> ListRecords :<|> CreateRecord :<|> UpdateRecord

listZones :: Config -> ClientM (Result [Zone])
listRecords :: Config -> Text -> ClientM (Result [Record])
createRecord :: Config -> Text -> Record -> ClientM (Result Record)
updateRecord :: Config -> Text -> Text -> Record -> ClientM (Result Record)

listZones :<|> listRecords :<|> createRecord :<|> updateRecord
  = client (Proxy.Proxy :: Proxy.Proxy API)

update :: ClientEnv -> Config -> IO ()
update env config@Config{..} = do
  iface <- listToMaybe . filterInterface <$> getNetworkInterfaces
  zone <- listToMaybe . filterZone . resultToList <$> runClient_ env (listZones config)
  case (iface, zone) of
    (Just NetworkInterface{..}, Just Zone{..}) -> void $ runClient_ env $ do
      let ip = T.pack (show ipv4)
      Result records <- listRecords config zoneId
      case filterRecord records of
        (record@Record{..}:_) ->
          updateRecord config zoneId recordId $ record { recordContent = ip }
        [] ->
          createRecord config zoneId $ Record "" domain ip
    rs -> error (show rs)
  where
    filterInterface  = filter (\NetworkInterface{..} -> name == interface)
    filterZone       = filter (\Zone{..} -> T.isSuffixOf zoneName domain)
    filterRecord     = filter (\Record{..} -> recordName == domain)

resultToList :: Either ClientError (Result [a]) -> [a]
resultToList = either (const []) result

dropFieldLabelPrefix :: Options
dropFieldLabelPrefix = defaultOptions { fieldLabelModifier = dropPrefix }

dropPrefix :: String -> String
dropPrefix [] = []
dropPrefix (x:xs)
  | isUpper x = toLower x : xs
  | otherwise = dropPrefix xs

runClient_ :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient_ = flip runClientM
