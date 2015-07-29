{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playlistach.Soundcloud (searchTracks, withAudioStream) where

import Data.Aeson
import Control.Monad (mzero)
import Control.Monad.Trans.Either (EitherT)
import Servant
import Servant.Client
import Servant.Common.Req (ServantError)
import Network.HTTP.Types.Method (methodGet)
import Playlistach.ServantExt
import Playlistach.Types

data ScTrack = ScTrack
    { scTrackId       :: String
    , scTrackTitle    :: String
    , scTrackDuration :: Int
    , scTrackUrl      :: String }

instance FromJSON ScTrack where
    parseJSON (Object v) =
        ScTrack <$> v .: "id"
                <*> v .: "title"
                <*> v .: "duration"
                <*> v .: "permalink_url"
    parseJSON _  = mzero

type ClientIdParam = RequiredParam "client_id" String

type API r = "tracks" :> RequiredParam "q" String :> ClientIdParam :> Get '[JSON] [ScTrack]
        :<|> "tracks" :> Capture "id" String :> "stream" :> ClientIdParam :> RawPipe r

_searchTracks :<|> _withAudioStream =
    client (Proxy :: Proxy (API r)) $
        BaseUrl Https "api.soundcloud.com" 8081

searchTracks :: String -> String -> EitherT ServantError IO [Track]
searchTracks query clientId = map toTrack <$> _searchTracks query clientId
  where
    toTrack ScTrack{..} = Track
        { trackId       = scTrackId
        , trackTitle    = scTrackTitle
        , trackDuration = scTrackDuration
        , trackUrl      = scTrackUrl
        , trackOrigin   = SC }

withAudioStream :: Track -> String -> (ProducerResponse -> IO r) -> EitherT ServantError IO r
withAudioStream Track{..} clientId streamer =
    _withAudioStream trackId clientId methodGet streamer