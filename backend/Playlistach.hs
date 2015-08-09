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
import           Playlistach.Util.List  as L
import           Playlistach.Model.Track
import qualified Playlistach.Mpeg       as Mpeg
import qualified Playlistach.Vk         as Vk
import qualified Playlistach.Soundcloud as Sc
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

search :: Manager -> Conf -> String -> EitherT ServantErr IO [Track]
search connManager Conf{..} query = liftIO $ do
    vkResults <- liftIO $ Vk.searchTracks vkLogin vkPassword query
    scResults <- Sc.searchTracks scCliendId query
    return $ L.proportionate [vkResults, scResults]

streamUrl :: Manager -> String -> (Wai.Response -> IO r) -> IO r
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

streamTemporary :: Manager -> String -> (Wai.Response -> IO r) -> IO r
streamTemporary connManager id respond = undefined

type API = "api" :> "search" :> RequiredParam "query" String :> Get '[JSON] [ClientTrack]
      :<|> "api" :> "stream"           :> RequiredParam "id" Int    :> Raw
      :<|> "api" :> "stream" :> "temp" :> RequiredParam "id" String :> Raw

server :: Manager -> Conf -> Server API
server connManager conf@Conf{..} =
    api_search :<|> api_stream :<|> api_stream_temp
  where
    api_search query = coerce $ search connManager conf query
    api_stream id _ respond = undefined
    api_stream_temp id _ respond = streamTemporary connManager id respond

main = do
    conf <- readConf "./app.conf"
    connManager <- newManager defaultManagerSettings
    Warp.runTLS tlsSettings Warp.defaultSettings $
        Wai.staticPolicy (Wai.addBase "./frontend") $
            serve api (server connManager conf)
  where
    api :: Servant.Proxy API
    api = Servant.Proxy
