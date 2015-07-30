{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playlistach.Soundcloud (searchTracks, withAudioStream) where

import Data.Coerce
import Data.Aeson
import Control.Monad (mzero)
import Control.Monad.Trans.Either (EitherT(..))
import Servant
import Servant.Client
import Servant.Common.Req (ServantError)
import Network.HTTP.Types.Method (methodGet)
import Playlistach.ServantExt
import Playlistach.Types

newtype ScTrack = ScTrack Track

instance FromJSON ScTrack where
    parseJSON (Object v) = coerce $
        Track <$> v .: "id"
              <*> v .: "title"
              <*> v .: "duration"
              <*> v .: "permalink_url"
              <*> pure SC
    parseJSON _  = mzero

type ClientIdParam = RequiredParam "client_id" String

type API = "tracks" :> RequiredParam "q" String :> ClientIdParam :> Get '[JSON] [ScTrack]
      :<|> "tracks" :> Capture "id" String :> "stream" :> ClientIdParam :> RawPipe

_searchTracks :<|> _withAudioStream =
    client (Proxy :: Proxy API) $
        BaseUrl Https "api.soundcloud.com" 8081

searchTracks :: String -> String -> EitherT ServantError IO [Track]
searchTracks query clientId = coerce $ _searchTracks query clientId

withAudioStream :: Track -> String -> (ProducerResponse -> IO r) -> EitherT ServantError IO r
withAudioStream Track{..} clientId streamer =
    runRawPipe (_withAudioStream trackId clientId methodGet) streamer