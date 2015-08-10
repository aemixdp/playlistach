{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Playlistach.ServantExt (RequiredParam) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Typeable (Typeable)
import Data.String.Conversions (cs)
import Control.Exception (Exception)
import Network.HTTP.Client       as HTTP hiding (Proxy)
import Network.HTTP.Types.Status as HTTP
import Network.HTTP.Types.URI (parseQueryText)
import Network.Wai (rawQueryString)
import Servant
import Servant.Server
import Servant.Server.Internal
import Servant.Client
import Servant.Common.Req

instance Exception ServantError

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

instance (KnownSymbol sym, ToText a, HasClient sublayout) =>
    HasClient (RequiredParam sym a :> sublayout)
  where
    type Client (RequiredParam sym a :> sublayout) =
        a -> Client sublayout

    clientWithRoute Proxy req baseurl param =
        clientWithRoute (Proxy :: Proxy sublayout) req' baseurl
      where
        req' = appendToQueryString pname (Just (toText param)) req
        pname = cs $ symbolVal (Proxy :: Proxy sym)
