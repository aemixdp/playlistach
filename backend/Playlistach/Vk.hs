{-# LANGUAGE LambdaCase #-}

module Playlistach.Vk (searchTracks) where

import qualified Web.VKHS.Types      as Vk
import qualified Web.VKHS.Login      as Vk
import qualified Web.VKHS.API        as Vk
import qualified Web.VKHS.API.Types  as Vk
import qualified Network.HTTP.Client as HTTP
import qualified Pipes.HTTP          as Pipes
import           Playlistach.Common
import           Playlistach.Model.Track  as Track
import           Playlistach.Model.Origin as Origin

vk :: String -> String -> (Vk.Env Vk.CallEnv -> IO (Either Vk.APIError r)) -> IO r
vk user pass cmd = either (error . show) id <$> do
    let loginEnv = Vk.env "111111" user pass [Vk.Audio]
    Vk.login loginEnv >>= \case
        Left e              -> error e
        Right (token, _, _) -> cmd (Vk.callEnv loginEnv token)

searchTracks :: String -> String -> String -> IO [Track]
searchTracks user pass query = vk user pass $ \env ->
    fmap entries <$> Vk.api' env "audio.search" options
  where
    entries (Vk.Response (Vk.SL _ es)) = map toTrack es
    toTrack r = Track
        { externalId   = show (Vk.owner_id r) ++ "_" ++ show (Vk.aid r)
        , title        = Vk.artist r ++ " - " ++ Vk.title r
        , duration     = Vk.duration r
        , streamUrl    = Just $ Vk.url r
        , permalinkUrl = Nothing
        , origin       = Origin.VK }
    options = [ ("q", query)
              , ("auto_complete", "1")
              , ("lyrics", "0")
              , ("performer_only", "0")
              , ("sort", "1")
              , ("search_own", "1")
              , ("offset", "0")
              , ("count", "10") ]
