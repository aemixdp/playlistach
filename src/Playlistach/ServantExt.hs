{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Playlistach.ServantExt (RequiredParam) where

import Data.Typeable (Typeable)
import Data.String.Conversions (cs)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Network.HTTP.Types.Status (status404)
import Network.HTTP.Types.URI (parseQueryText)
import Network.Wai (rawQueryString)
import Servant
import Servant.Server
import Servant.Server.Internal

data RequiredParam (sym :: Symbol) a
  deriving (Typeable)

instance (KnownSymbol sym, FromText a, HasServer sublayout) =>
    HasServer (RequiredParam sym a :> sublayout)
  where
    type ServerT (RequiredParam sym a :> sublayout) m =
        a -> ServerT sublayout m

    route Proxy subserver request respond =
        case lookup paramName queryText of
            Just (Just (fromText -> Just v)) -> route proxy (subserver v) request respond
            _ -> respond $ failWith $ HttpError status404 Nothing
      where
        queryText = parseQueryText $ rawQueryString request
        paramName = cs $ symbolVal (Proxy :: Proxy sym)
        proxy = Proxy :: Proxy sublayout
