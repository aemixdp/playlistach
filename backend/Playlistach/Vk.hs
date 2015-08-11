{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Playlistach.Vk (login, searchTracks, getStreamUrl) where

import           Data.Coerce
import           Data.Aeson
import           Data.Vector as V (tail)
import           Control.Monad
import           Control.Monad.Trans.Either (EitherT(..))
import qualified Web.VKHS.Types as Vk
import qualified Web.VKHS.Login as Vk
import           Servant
import           Servant.Client
import           Playlistach.ServantExt
import           Playlistach.Model.Track (Track)
import qualified Playlistach.Model.Track  as T
import           Playlistach.Model.Origin as Origin

newtype VkResponse a = VkResponse a
newtype VkTrack = VkTrack Track

instance FromJSON a => FromJSON (VkResponse a) where
    parseJSON (Object v) = do
        Array items <- v .: "response"
        items <- parseJSON $ Array $ V.tail items
        return $ VkResponse items
    parseJSON _  = mzero

instance FromJSON VkTrack where
    parseJSON (Object v) = do
        id       <- v .: "aid"
        ownerId  <- v .: "owner_id"
        artist   <- v .: "artist"
        title    <- v .: "title"
        duration <- v .: "duration"
        url      <- v .: "url"
        return $ VkTrack $ T.Track
            { T.externalId   = show (ownerId :: Integer) ++ "_" ++ show (id :: Integer)
            , T.title        = artist ++ " - " ++ title
            , T.duration     = duration
            , T.streamUrl    = url
            , T.permalinkUrl = Nothing
            , T.origin       = Origin.VK }
    parseJSON _  = mzero

type API = "method" :> "audio.search"
    :> RequiredParam "access_token" String
    :> RequiredParam "q" String
    :> Get '[JSON] (VkResponse [VkTrack])

_searchTracks = client (Proxy :: Proxy API) $ BaseUrl Https "api.vk.com" 443

login :: String -> String -> String -> IO String
login clientId user pass = do
    let loginEnv = Vk.env clientId user pass [Vk.Audio]
    Vk.login loginEnv >>= \case
        Left e              -> error e
        Right (token, _, _) -> return token

searchTracks :: String -> String -> IO [Track]
searchTracks accessToken query = runServantClient $
    coerce $ _searchTracks accessToken query

getStreamUrl :: Track -> String
getStreamUrl = maybe (error "Vk track without streamUrl!") id . T.streamUrl
