{-# LANGUAGE DeriveGeneric #-}

module Playlistach.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data Origin =
    VK | SC
  deriving
    (Eq, Show, Generic)

data Track = Track
    { trackId       :: String
    , trackTitle    :: String
    , trackDuration :: Int
    , trackUrl      :: String
    , trackOrigin   :: Origin }
  deriving
    (Generic)

instance ToJSON Origin
instance ToJSON Track