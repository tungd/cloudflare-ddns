{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, UndecidableInstances, GADTs #-}
module Lib where

import RIO hiding (id)
import RIO.List (find)

import Data.Aeson hiding (Result)
import Network.DNS
import Network.Info
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core

import qualified Data.Proxy as Proxy
import qualified RIO.Text as T


data Credentials = AuthToken Text | ApiKey Text Text
  deriving (Show, Generic, Typeable)

data Interface = Public | Private Text
  deriving (Show, Generic)

instance FromJSON Interface where
  parseJSON = withText "Interface" $ \case
    "public" -> pure Public
    t        -> pure (Private t)

data Config = Config
  { domain      :: !Text
  , interface   :: !Interface
  , credentials :: !Credentials
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    domain      <- o .: "domain"
    interface   <- o .: "interface"
    credentials <- liftA3 mkcredentials
      (o .:? "auth") (o .:? "email") (o .:? "apikey")
    pure Config{..}
    where
      mkcredentials (Just token) _ _                   = AuthToken token
      mkcredentials Nothing (Just email) (Just apikey) = ApiKey email apikey
      mkcredentials _ _ _                              = error "credentials is required"

newtype Result a = Result { result :: a }
  deriving (Show)

instance FromJSON a => FromJSON (Result a) where
  parseJSON = withObject "result" $ \o -> Result <$> o .: "result"

data Record = Record
  { id      :: !Text
  , name    :: !Text
  , content :: !Text
  } deriving (Show, Generic)

instance ToJSON Record where
  toJSON Record{..} = object
    [ "type"    .= ("A" :: Text)
    , "proxied" .= False
    , "name"    .= name
    , "content" .= content
    ]

instance FromJSON Record

data Zone = Zone { id :: Text, name :: Text }
  deriving (Show, Generic)

instance FromJSON Zone

instance (HasClient m api) => HasClient m (Credentials :> api) where
  type Client m (Credentials :> api) = Config -> Client m api

  clientWithRoute m _ req Config{..} = case credentials of
    ApiKey email apikey ->
      clientWithRoute m (Proxy.Proxy @api) $ req
      & addHeader "X-Auth-Email" email
      & addHeader "X-Auth-Key" apikey
    AuthToken token ->
      clientWithRoute m (Proxy.Proxy @api) $ req
      & addHeader "Authorization" ("Bearer " <> token)

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
  = client (Proxy.Proxy @API)

showText :: Show a => a -> Text
showText = T.pack . show

resolve :: Interface -> IO (Maybe Text)
resolve Public = do
  base <- makeResolvSeed defaultResolvConf
  rs <- withResolver base $ \resolver -> lookupA resolver "resolver1.opendns.com"
  case rs of
    Right opendns -> do
      custom <- makeResolvSeed defaultResolvConf
        { resolvInfo = RCHostNames (show <$> opendns) }
      myip <- withResolver custom $ \resolver -> lookupA resolver "myip.opendns.com"
      pure $ either (const Nothing) (fmap showText . listToMaybe) myip
    _ -> pure Nothing
resolve (Private target) =
  fmap (showText . ipv4)
  . find (\iface -> T.pack (Network.Info.name iface) == target) <$> getNetworkInterfaces

update :: ClientEnv -> Config -> IO ()
update env config@Config{..} = do
  iface <- resolve interface
  mzone <- find (\zone -> T.isSuffixOf zone.name domain)
    . resultToList <$> runClient_ env (listZones config)
  case (iface, mzone) of
    (Just ip, Just zone) -> void $ runClient_ env $ do
      Result records <- listRecords config zone.id
      case find (\record -> record.name == domain) records of
        Just record ->
          updateRecord config zone.id record.id record{content = ip}
        Nothing ->
          createRecord config zone.id (Record "" domain ip)
    rs -> error (show rs)

resultToList :: Either ClientError (Result [a]) -> [a]
resultToList = either (const []) result

runClient_ :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient_ = flip runClientM
