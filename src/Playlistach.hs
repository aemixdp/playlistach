{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playlistach where

import           System.IO
import           Control.Monad
import           Data.Maybe (isJust)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Builder       as BB
import           Pipes
import           Pipes.Safe                    as Pipes
import           Network.HTTP.Client           as HTTP
import           Network.HTTP.Types.Status     as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Middleware.Static as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WarpTLS   as Warp
import           Database.Redis (runRedis)
import qualified Database.Redis                as Redis
import           Servant
import           Servant.API                   as Servant

import           Playlistach.Types
import qualified Playlistach.Mpeg as Mpeg
import qualified Playlistach.Vk   as Vk
import qualified Playlistach.Soundcloud
import           Playlistach.ServantExt

data Conf = Conf
    { vkLogin    :: String
    , vkPassword :: String
    , scCliendId :: String }

readConf :: FilePath -> IO Conf
readConf path =
    withFile path ReadMode $ \h -> do
        Conf <$> hGetLine h
             <*> hGetLine h
             <*> hGetLine h

tlsSettings :: Warp.TLSSettings
tlsSettings = Warp.defaultTlsSettings
    { Warp.certFile = "cert.pem"
    , Warp.keyFile  = "key.pem" }

type API = "api" :> "search" :> RequiredParam "query" String :> Get '[JSON] [Track]
      :<|> "api" :> "audio"  :> RequiredParam "id"    String :> Raw

server :: Conf -> Manager -> Server API
server Conf{..} connManager =
    api_search :<|> api_audio
  where
    api_search query = liftIO $ Vk.exec vkLogin vkPassword (Vk.searchTracks query)
    api_audio url = undefined

main = do
    conf <- readConf "./app.conf"
    redisConn <- Redis.connect Redis.defaultConnectInfo
    withManager defaultManagerSettings $ \connMgr -> do
        Warp.runTLS tlsSettings Warp.defaultSettings $
            Wai.staticPolicy (Wai.addBase "./web") $
                serve api (server conf connMgr)
  where
    api :: Servant.Proxy API
    api = Servant.Proxy
