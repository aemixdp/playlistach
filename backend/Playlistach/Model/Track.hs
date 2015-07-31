{-# LANGUAGE DeriveGeneric #-}

module Playlistach.Model.Track (Track(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Playlistach.Model.Origin

data Track = Track
    { tid      :: String
    , title    :: String
    , duration :: Int
    , url      :: String
    , origin   :: Origin }
  deriving
    (Generic)

instance ToJSON Track