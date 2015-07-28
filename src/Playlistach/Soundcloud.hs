{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playlistach.Soundcloud (searchTracks, streamTrack) where

import Data.Aeson
import Control.Monad (mzero)
import Control.Monad.Trans.Either (EitherT)
import Servant
import Servant.Client
import Servant.Common.Req (ServantError)
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

type API = "tracks" :> RequiredParam "q" String :> ClientIdParam :> Get '[JSON] [ScTrack]
      :<|> "tracks" :> Capture "id" String :> "stream" :> ClientIdParam :> Raw

_searchTracks :<|> _streamTrack =
    client (Proxy :: Proxy API) $
        BaseUrl Https "api.soundcloud.com" 8081

searchTracks :: String -> String -> EitherT ServantError IO [Track]
searchTracks q cid = map toTrack <$> _searchTracks q cid
  where
    toTrack ScTrack{..} = Track
        { trackId       = scTrackId
        , trackTitle    = scTrackTitle
        , trackDuration = scTrackDuration
        , trackUrl      = scTrackUrl
        , trackOrigin   = SC }

streamTrack tid cid = _streamTrack tid cid
