{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Playlistach.Soundcloud (searchTracks, getStreamUrl) where

import           Data.Coerce
import           Data.Aeson
import           Control.Monad (mzero)
import           Control.Monad.Trans.Either (EitherT(..), eitherT)
import           Control.Exception (throwIO)
import           Network.HTTP.Types.Method (methodGet)
import           Servant
import           Servant.Client
import qualified Servant.Common.Req as ServantCli
import           Formatting
import           Playlistach.Common
import           Playlistach.ServantExt
import           Playlistach.Model.Track (Track)
import qualified Playlistach.Model.Track  as T
import qualified Playlistach.Model.Origin as Origin

newtype ScTrack = ScTrack Track

instance FromJSON ScTrack where
    parseJSON (Object v) = do
        id       <- v .: "id"
        title    <- v .: "title"
        duration <- v .: "duration"
        url      <- v .: "permalink_url"
        let toScTrack :: Track -> ScTrack
            toScTrack = coerce
        return $ toScTrack $ T.Track
            { T.externalId   = show (id :: Int)
            , T.title        = title
            , T.duration     = duration `div` 1000
            , T.streamUrl    = Nothing
            , T.permalinkUrl = Just url
            , T.origin       = Origin.SC }
    parseJSON _  = mzero

type API = "tracks" :> RequiredParam "q" String :> RequiredParam "client_id" String :> Get '[JSON] [ScTrack]

_searchTracks = client (Proxy :: Proxy API) $ BaseUrl Https "api.soundcloud.com" 443

runCli :: EitherT ServantCli.ServantError IO r -> IO r
runCli = eitherT throwIO return

searchTracks :: String -> String -> IO [Track]
searchTracks clientId query = runCli $ coerce $ _searchTracks query clientId

getStreamUrl :: String -> Track -> String
getStreamUrl clientId track = formatToString template (T.externalId track) clientId
  where
    template = "https://api.soundcloud.com/tracks/" % string % "/stream?client_id=" % string
