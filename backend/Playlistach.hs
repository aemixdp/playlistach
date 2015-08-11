{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playlistach where

import           System.IO
import           Data.Coerce
import           Data.Maybe (isJust)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Builder       as BB
import           Control.Monad
import           Control.Monad.Trans.Either (EitherT(..))
import           Pipes
import qualified Pipes.HTTP                    as Pipes
import           Network.HTTP.Client           as HTTP
import           Network.HTTP.Types.Status     as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Middleware.Static as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WarpTLS   as Warp
import           Servant
import           Servant.API                   as Servant
import           Database.Redis                as Redis
import           Playlistach.Util.List         as L
import           Playlistach.Model.Track (Track, ClientTrack)
import qualified Playlistach.Model.Track       as Track
import qualified Playlistach.Mpeg              as Mpeg
import qualified Playlistach.Vk                as Vk
import qualified Playlistach.Soundcloud        as Sc
import           Playlistach.ServantExt

search :: HTTP.Manager -> Redis.Connection -> Conf -> String -> EitherT ServantErr IO [Track]
search connManager redisConn Conf{..} query = liftIO $ do
    vkResults <- liftIO $ Vk.searchTracks vkLogin vkPassword query
    scResults <- Sc.searchTracks scCliendId query
    let getIdAndStreamUrl getUrl track = (Track.externalId track, getUrl track)
    let urlMappings = map (getIdAndStreamUrl Vk.getStreamUrl) vkResults
                   ++ map (getIdAndStreamUrl (Sc.getStreamUrl scCliendId)) scResults
    runRedis redisConn $ do
        forM_ urlMappings $ \(id, url) ->
            Redis.setex (BS.pack id) urlCacheExpireSeconds (BS.pack url)
    return $ L.proportionate [vkResults, scResults]

streamUrl :: HTTP.Manager -> String -> (Wai.Response -> IO r) -> IO r
streamUrl connManager url respond = do
    req <- parseUrl url
    Pipes.withHTTP req connManager $ \res -> do
        case statusCode (responseStatus res) of
            200 -> do
                (mph, chunks) <- Mpeg.findHeader (responseBody res)
                let headers = [ ("Content-Type", "audio/mpeg")
                              , ("Bitrate", maybe "VBR" (BS.pack . show) $ Mpeg.bitrate mph)
                              , ("Frequency", BS.pack $ show $ Mpeg.frequency mph) ]
                respond $ Wai.responseStream status200 headers $ \write flush -> do
                    runEffect $ for chunks $ \chunk -> liftIO $ do
                        write $ BB.byteString chunk
                        flush
            _ -> respond $
                Wai.responseBuilder status404 [] mempty

streamTemporary :: HTTP.Manager -> Redis.Connection -> String -> (Wai.Response -> IO r) -> IO r
streamTemporary connManager redisConn id respond = do
    Right mbUrl <- runRedis redisConn $ Redis.get (BS.pack id)
    case mbUrl of
        Just url -> streamUrl connManager (BS.unpack url) respond
        Nothing  -> error "Cached stream url expired!"

type API = "api" :> "search" :> RequiredParam "query" String :> Get '[JSON] [ClientTrack]
      :<|> "api" :> "stream"           :> RequiredParam "id" Int    :> Raw
      :<|> "api" :> "stream" :> "temp" :> RequiredParam "id" String :> Raw

server :: HTTP.Manager -> Redis.Connection -> Conf -> Server API
server connManager redisConn conf@Conf{..} =
    api_search :<|> api_stream :<|> api_stream_temp
  where
    api_search query = coerce $ search connManager redisConn conf query
    api_stream id _ respond = undefined
    api_stream_temp id _ respond = streamTemporary connManager redisConn id respond

data Conf = Conf
    { vkLogin               :: String
    , vkPassword            :: String
    , scCliendId            :: String
    , urlCacheExpireSeconds :: Integer }

readConf :: FilePath -> IO Conf
readConf path =
    withFile path ReadMode $ \h -> do
        Conf <$> hGetLine h
             <*> hGetLine h
             <*> hGetLine h
             <*> fmap read (hGetLine h)

tlsSettings :: Warp.TLSSettings
tlsSettings = Warp.defaultTlsSettings
    { Warp.certFile = "cert.pem"
    , Warp.keyFile  = "key.pem" }

redisConnInfo :: Redis.ConnectInfo
redisConnInfo = Redis.defaultConnectInfo

main = do
    conf <- readConf "./app.conf"
    connManager <- newManager defaultManagerSettings
    redisConn <- Redis.connect redisConnInfo
    Warp.runTLS tlsSettings Warp.defaultSettings $
        Wai.staticPolicy (Wai.addBase "./frontend") $
            serve (Servant.Proxy :: Servant.Proxy API) $
                server connManager redisConn conf
