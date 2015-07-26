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

import qualified Playlistach.Mpeg as Mpeg
import qualified Playlistach.Vk   as Vk
import           Playlistach.ServantExt

requestPipe :: Manager -> Request -> IO (Response (Producer ByteString (SafeT IO) ()))
requestPipe mgr req = do
    res <- responseOpen req mgr
    return $ res { responseBody = bodyProducer res }
  where
    bodyProducer res = do
        register $ liftIO $ responseClose res
        consumeResponseBody $ brRead $ responseBody res
    consumeResponseBody readChunk = loop
      where
        loop = liftIO readChunk >>= \chunk ->
            unless (BS.null chunk) $ yield chunk >> loop

streamAudio :: Manager -> String -> Wai.Request -> (Wai.Response -> IO r) -> IO r
streamAudio connManager url _ respond = do
    req <- parseUrl url
    res <- requestPipe connManager req
    case statusCode (responseStatus res) of
        200 -> do
            let headers = [("Content-Type", "audio/mpeg")]
            respond $ Wai.responseStream status200 headers $ \write flush -> do
                runSafeT $ runEffect $ do
                    (mph, chunks) <- lift $ Mpeg.headerFromPipe (responseBody res)
                    liftIO $ putStrLn ("Header: " ++ show mph)
                    for chunks $ \chunk -> liftIO $ do
                        write $ BB.byteString chunk
                        flush
        _ -> respond $
            Wai.responseBuilder status404 [] mempty

data AuthConf = AuthConf
  { vkLogin    :: String
  , vkPassword :: String }

readAuthConf :: FilePath -> IO AuthConf
readAuthConf path =
    withFile path ReadMode $ \h -> do
        AuthConf <$> hGetLine h
                 <*> hGetLine h

tlsSettings :: Warp.TLSSettings
tlsSettings = Warp.defaultTlsSettings
    { Warp.certFile = "cert.pem"
    , Warp.keyFile  = "key.pem" }

type API = "api" :> "search" :> RequiredParam "query" String :> Get '[JSON] [Vk.Song]
      :<|> "api" :> "audio"  :> RequiredParam "url"   String :> Raw

server :: AuthConf -> Manager -> Server API
server AuthConf{..} connManager =
    api_search :<|> api_audio
  where
    api_search query = liftIO $ Vk.exec vkLogin vkPassword (Vk.searchAudio query)
    api_audio url = streamAudio connManager url

main = do
    authConf <- readAuthConf "./auth.conf"
    redisConn <- Redis.connect Redis.defaultConnectInfo
    withManager defaultManagerSettings $ \connMgr -> do
        Warp.runTLS tlsSettings Warp.defaultSettings $
            Wai.staticPolicy (Wai.addBase "./web") $
                serve api (server authConf connMgr)
  where
    api :: Servant.Proxy API
    api = Servant.Proxy
