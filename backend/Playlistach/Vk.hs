{-# LANGUAGE LambdaCase #-}

module Playlistach.Vk (searchTracks, withAudioStream, exec) where

import qualified Web.VKHS.Types      as Vk
import qualified Web.VKHS.Login      as Vk
import qualified Web.VKHS.API        as Vk
import qualified Web.VKHS.API.Types  as Vk
import qualified Network.HTTP.Client as HTTP
import qualified Pipes.HTTP          as Pipes
import           Playlistach.Common
import           Playlistach.Model.Track  as Track
import           Playlistach.Model.Origin as Origin

vk :: String -> String -> (Vk.Env Vk.CallEnv -> IO a) -> IO a
vk user pass cmd = do
    let loginEnv = Vk.env "111111" user pass [Vk.Audio]
    Vk.login loginEnv >>= \case
        Left e              -> error e
        Right (token, _, _) -> cmd (Vk.callEnv loginEnv token)

exec :: Show a => String -> String -> (Vk.Env Vk.CallEnv -> IO (Either a b)) -> IO b
exec user pass cmd = either (error . show) id <$> vk user pass cmd

searchTracks :: String -> Vk.Env Vk.CallEnv -> IO (Either Vk.APIError [Track])
searchTracks query env =
    fmap entries <$> Vk.api' env "audio.search" options
  where
    entries (Vk.Response (Vk.SL _ es)) = map toTrack es

    toTrack r = Track
        { tid      = show (Vk.owner_id r) ++ "_" ++ show (Vk.aid r)
        , title    = Vk.artist r ++ " - " ++ Vk.title r
        , duration = Vk.duration r
        , url      = Vk.url r
        , origin   = Origin.VK }

    options = [ ("q", query)
              , ("auto_complete", "1")
              , ("lyrics", "0")
              , ("performer_only", "0")
              , ("sort", "1")
              , ("search_own", "1")
              , ("offset", "0")
              , ("count", "10") ]

withAudioStream :: Track -> HTTP.Manager -> (ProducerResponse -> IO r) -> IO r
withAudioStream track connManager streamer = do
    request <- HTTP.parseUrl (Track.url track)
    Pipes.withHTTP request connManager streamer