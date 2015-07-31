{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}

module Playlistach.ServantExt (RequiredParam, RawPipe(..)) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import Data.String.Conversions (cs)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Catch (catch)
import Network.HTTP.Client       as HTTP hiding (Proxy)
import Network.HTTP.Types        as HTTP
import Network.HTTP.Types.Status as HTTP
import Network.HTTP.Media ((//), parseAccept, MediaType)
import Network.HTTP.Types.URI (parseQueryText)
import Network.Wai (rawQueryString)
import Servant
import Servant.Server
import Servant.Server.Internal
import Servant.Client
import Servant.Common.Req
import Pipes (Producer)
import Pipes.HTTP hiding (Proxy)
import Playlistach.Common

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

newtype RawPipe = RawPipe
    { runRawPipe :: forall r . (ProducerResponse -> IO r) -> EitherT ServantError IO r }
  deriving (Typeable)

instance HasClient RawPipe where
    type Client RawPipe = Method -> RawPipe

    clientWithRoute Proxy req baseurl httpMethod =
        performStreamingRequest httpMethod req (const True) baseurl

performStreamingRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl -> RawPipe
performStreamingRequest reqMethod req isWantedStatus baseUrl =
    RawPipe $ \streamer -> do
        partialRequest <- liftIO $ reqToRequest req baseUrl
        let request = partialRequest { HTTP.method = reqMethod, checkStatus = \_ _ _ -> Nothing }
        EitherT $ __withGlobalManager $ \manager -> catchConnectionError $
            withHTTP request manager $ \response -> runEitherT $ do
                ct <- extractContentType response
                let status = HTTP.responseStatus response
                unless (isWantedStatus (statusCode status)) $
                    left $ FailureResponse status ct mempty
                liftIO $ streamer response
  where
    extractContentType response =
        case lookup "Content-Type" (HTTP.responseHeaders response) of
            Nothing -> return $ "application" // "octet-stream"
            Just t -> case parseAccept t of
                Nothing -> left $ InvalidContentTypeHeader (cs t) mempty
                Just t' -> return t'

    catchConnectionError action =
        catch action $ \(e :: HttpException) ->
            return $ Left $ ConnectionError e
