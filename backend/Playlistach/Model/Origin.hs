{-# LANGUAGE DeriveGeneric #-}

module Playlistach.Model.Origin (Origin(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data Origin =
    VK | SC
  deriving
    (Eq, Show, Generic)

instance ToJSON Origin