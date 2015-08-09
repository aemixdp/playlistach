{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Playlistach.Model.Track where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Profunctor.Product.TH
import Control.Arrow
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField
import Opaleye
import Playlistach.Model.WithId
import Playlistach.Model.Origin as Origin

data Track' a b c d e f = Track
    { externalId   :: a
    , title        :: b
    , duration     :: c
    , streamUrl    :: d
    , permalinkUrl :: e
    , origin       :: f }

makeAdaptorAndInstance "pTrack" ''Track'

type Track = Track' String String Int (Maybe String) (Maybe String) Origin

type TrackColumn = Track'
    (Column PGText)            (Column PGText)            (Column PGInt4)
    (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGInt4)

newtype ClientTrack = ClientTrack Track

instance ToJSON ClientTrack where
    toJSON (ClientTrack Track{..}) = object
        [ "externalId"   .= externalId
        , "title"        .= title
        , "duration"     .= duration
        , "permalinkUrl" .= permalinkUrl
        , "origin"       .= toJSON origin ]

instance FromField Origin where
    fromField field mbBS = fmap f (fromField field mbBS)
      where
        f :: Int -> Origin
        f 0 = Origin.VK
        f 1 = Origin.SC

instance QueryRunnerColumnDefault PGInt4 Origin where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

table :: Table (WithIdColumn TrackColumn) (WithIdColumn TrackColumn)
table = Table "tracks" $ withId $ pTrack Track
    { externalId   = required "external_id"
    , title        = required "title"
    , duration     = required "duration"
    , streamUrl    = required "stream_url"
    , permalinkUrl = required "permalink_url"
    , origin       = required "origin" }

queryAll :: Query (WithIdColumn TrackColumn)
queryAll = queryTable table

runQ :: Connection -> Query TrackColumn -> IO [Track]
runQ = runQuery
