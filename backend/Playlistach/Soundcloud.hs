{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Playlistach.Soundcloud (searchTracks, withAudioStream) where

import Data.Coerce
import Data.Aeson
import Control.Monad (mzero)
import Control.Monad.Trans.Either (EitherT(..), eitherT)
import Control.Exception (throwIO)
import Servant
import Servant.Client
import Servant.Common.Req as ServantCli
import Network.HTTP.Types.Method (methodGet)
import Playlistach.Common
import Playlistach.ServantExt
import Playlistach.Model.Track  as Track
import Playlistach.Model.Origin as Origin

newtype ScTrack = ScTrack Track

instance FromJSON ScTrack where
    parseJSON (Object v) = do
        id       <- v .: "id"
        title    <- v .: "title"
        duration <- v .: "duration"
        url      <- v .: "permalink_url"
        let toScTrack :: Track -> ScTrack
            toScTrack = coerce
        return $ toScTrack $ Track
            { externalId   = id
            , title        = title
            , duration     = duration
            , streamUrl    = Nothing
            , permalinkUrl = Just url
            , origin       = Origin.SC }
    parseJSON _  = mzero

type ClientIdParam = RequiredParam "client_id" String

type API = "tracks" :> RequiredParam "q" String :> ClientIdParam :> Get '[JSON] [ScTrack]
      :<|> "tracks" :> Capture "id" String :> "stream" :> ClientIdParam :> RawPipe

_searchTracks :<|> _withAudioStream =
    client (Proxy :: Proxy API) $
        BaseUrl Https "api.soundcloud.com" 8081

runCli :: EitherT ServantCli.ServantError IO r -> IO r
runCli = eitherT throwIO return

searchTracks :: String -> String -> IO [Track]
searchTracks clientId query = runCli $
    coerce $ _searchTracks query clientId

withAudioStream :: Track -> String -> (ProducerResponse -> IO r) -> EitherT ServantError IO r
withAudioStream track clientId streamer =
    runRawPipe (_withAudioStream (Track.externalId track) clientId methodGet) streamer
