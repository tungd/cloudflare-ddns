module Lib where

import Data.Aeson hiding (Result)
import Data.Proxy
import Network.Info
import RIO
import RIO.Char
import Servant.API
import Servant.Client
import System.IO

import qualified RIO.Text as T


data Config = Config
  { email :: Text
  , apiKey :: Text
  , domain :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Config

newtype Result a = Result { result :: a }
  deriving (Show)

instance FromJSON a => FromJSON (Result a) where
  parseJSON = withObject "result" $ \o -> Result <$> o .: "result"

data Record = Record
  { recordId :: Text
  , recordName :: Text
  , recordContent :: Text
  } deriving (Show, Generic)

instance ToJSON Record where
  toJSON Record{..} = object
    [ "type" .= ("A" :: Text)
    , "proxied" .= False
    , "name" .= recordName
    , "content" .= recordContent
    ]

instance FromJSON Record where
  parseJSON = genericParseJSON dropFieldLabelPrefix

data Zone = Zone { zoneId :: Text, zoneName :: Text }
  deriving (Show, Generic)

instance FromJSON Zone where
  parseJSON = genericParseJSON dropFieldLabelPrefix

type ListZones = "zones"
  :> Header "X-Auth-Email" Text
  :> Header "X-Auth-Key" Text
  :> Get '[JSON] (Result [Zone])

type ListRecords = "zones"
  :> Header "X-Auth-Email" Text
  :> Header "X-Auth-Key" Text
  :> Capture "zone_uuid" Text
  :> "dns_records"
  :> Get '[JSON] (Result [Record])

type CreateRecord = "zones"
  :> Header "X-Auth-Email" Text
  :> Header "X-Auth-Key" Text
  :> Capture "zone_uuid" Text
  :> "dns_records"
  :> ReqBody '[JSON] Record
  :> Post '[JSON] (Result Record)

type UpdateRecord = "zones"
  :> Header "X-Auth-Email" Text
  :> Header "X-Auth-Key" Text
  :> Capture "zone_uuid" Text
  :> "dns_records"
  :> Capture "record_uuid" Text
  :> ReqBody '[JSON] Record
  :> Patch '[JSON] (Result Record)

type API = ListZones :<|> ListRecords :<|> CreateRecord :<|> UpdateRecord

listZones ::
  Maybe Text -> Maybe Text -> ClientM (Result [Zone])
listRecords ::
  Maybe Text -> Maybe Text -> Text -> ClientM (Result [Record])
createRecord ::
  Maybe Text -> Maybe Text -> Text -> Record -> ClientM (Result Record)
updateRecord ::
  Maybe Text -> Maybe Text -> Text -> Text -> Record -> ClientM (Result Record)

listZones :<|> listRecords :<|> createRecord :<|> updateRecord
  = client (Proxy :: Proxy API)

update :: ClientEnv -> Config -> IO ()
update env Config{..} = do
  iface <- listToMaybe . filterInterface <$> getNetworkInterfaces
  zone <- listToMaybe . filterZone . resultToList <$> runClient_ listZones_
  case (iface, zone) of
    (Just NetworkInterface{..}, Just Zone{..}) -> do
      let ip = T.pack (show ipv4)
      result <- runClient_ $ do
        Result records <- listRecords_ zoneId
        case filterRecord records of
          (record@Record{..}:_) ->
            updateRecord_ zoneId recordId $ record { recordContent = ip }
          [] ->
            createRecord_ zoneId $ Record "" domain ip
      liftIO $ print result
    rs -> error (show rs)
  where
    runClient_ a = runClientM a env

    listZones_ = listZones (Just email) (Just apiKey)
    listRecords_ = listRecords (Just email) (Just apiKey)
    createRecord_ = createRecord (Just email) (Just apiKey)
    updateRecord_ = updateRecord (Just email) (Just apiKey)

    filterInterface = filter (\NetworkInterface{..} -> name == "en0")
    filterZone = filter (\Zone{..} -> T.isSuffixOf zoneName domain)
    filterRecord = filter (\Record{..} -> recordName == domain)

resultToList :: Either ServantError (Result [a]) -> [a]
resultToList = \case
  Right (Result rs) -> rs
  Left _            -> []

dropFieldLabelPrefix :: Options
dropFieldLabelPrefix = defaultOptions { fieldLabelModifier = dropPrefix }

dropPrefix :: String -> String
dropPrefix [] = []
dropPrefix (x:xs)
  | isUpper x = toLower x : xs
  | otherwise = dropPrefix xs
