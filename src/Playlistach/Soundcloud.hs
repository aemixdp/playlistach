{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Playlistach.Soundcloud (searchTracks, streamTrack) where

import Data.Aeson
import Control.Monad (mzero)
import Servant
import Servant.Client
import Playlistach.ServantExt

data Track = Track
    { trackId  :: Integer
    , duration :: Integer }

instance FromJSON Track where
    parseJSON (Object v) =
        Track <$> v .: "id"
              <*> v .: "duration"
    parseJSON _  = mzero

type ClientIdParam = RequiredParam "client_id" String

type API = "tracks" :> RequiredParam "q" String :> ClientIdParam :> Get '[JSON] [Track]
      :<|> "tracks" :> Capture "id" Integer :> "stream" :> ClientIdParam :> Raw

searchTracks :<|> streamTrack = client api (BaseUrl Https "api.soundcloud.com" 8081)

api :: Proxy API
api = Proxy