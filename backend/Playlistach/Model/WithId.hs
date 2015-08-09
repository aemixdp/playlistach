{-# LANGUAGE Arrows #-}

module Playlistach.Model.WithId where

import Data.Profunctor.Product (p2)
import Control.Arrow
import Opaleye

type WithId r = (Int, r)

type IdColumn = Column PGInt4

type WithIdColumn r = (IdColumn, r)

withId :: TableProperties a b -> TableProperties (Column id_t, a) (Column id_t, b)
withId r = p2 (required "id", r)

findById :: QueryArr () (WithIdColumn r) -> QueryArr IdColumn r
findById query = proc (id) -> do
    (id', entry) <- query -< ()
    restrict -< id' .== id
    returnA -< entry