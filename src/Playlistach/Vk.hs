{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Playlistach.Vk where

import           Data.Aeson ((.=))
import qualified Data.Aeson         as Aeson
import qualified Web.VKHS.Types     as Vk
import qualified Web.VKHS.Login     as Vk
import qualified Web.VKHS.API       as Vk
import qualified Web.VKHS.API.Types as Vk

searchAudio :: String -> Vk.Env Vk.CallEnv -> IO (Either Vk.APIError [Vk.MusicRecord])
searchAudio query env =
    fmap entries <$> Vk.api' env "audio.search" options
  where
    entries (Vk.Response (Vk.SL _ es)) = es
    options = [ ("q", query)
              , ("auto_complete", "1")
              , ("lyrics", "0")
              , ("performer_only", "0")
              , ("sort", "1")
              , ("search_own", "1")
              , ("offset", "0")
              , ("count", "10") ]

vk :: String -> String -> (Vk.Env Vk.CallEnv -> IO a) -> IO a
vk user pass cmd = do
    let loginEnv = Vk.env "111111" user pass [Vk.Audio]
    Vk.login loginEnv >>= \case
        Left e              -> error e
        Right (token, _, _) -> cmd (Vk.callEnv loginEnv token)

exec :: Show a => String -> String -> (Vk.Env Vk.CallEnv -> IO (Either a b)) -> IO b
exec user pass cmd = either (error . show) id <$> vk user pass cmd

musicRecordJson :: Vk.MusicRecord -> Aeson.Value
musicRecordJson Vk.MR{..} =
    Aeson.object [ "id"     .= (show owner_id ++ "_" ++ show aid)
                 , "artist" .= artist
                 , "title"  .= title
                 , "url"    .= url ]
