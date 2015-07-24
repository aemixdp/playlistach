{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Playlistach where

import           System.IO
import           Control.Monad
import           Data.Maybe (isJust)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Builder.Extra as BB
import           Pipes
import           Pipes.Safe                    as Pipes
import qualified Pipes.Safe.Prelude            as Pipes
import qualified Pipes.ByteString              as PipesBS
import           Network.HTTP.Client           as HTTP
import           Network.HTTP.Types.Status     as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Middleware.Static as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WarpTLS   as Warp
import           Web.Spock                     as Spock
import           Database.Redis (runRedis)
import qualified Database.Redis                as Redis

import qualified Playlistach.Mpeg as Mpeg
import qualified Playlistach.Vk   as Vk

requestPipe :: Manager -> Request -> IO (Response (Producer ByteString (SafeT IO) ()))
requestPipe mgr req = do
    res <- responseOpen req mgr
    return $ res { responseBody = bodyProducer res }
  where
    bodyProducer res = do
        register $ liftIO $ responseClose res
        consumeResponseBody $ brRead $ responseBody res
    consumeResponseBody readChunk = loop
      where
        loop = liftIO readChunk >>= \chunk ->
            unless (BS.null chunk) $ yield chunk >> loop

streamAudio :: Manager -> String -> ActionT (SafeT IO) ()
streamAudio mgr url = do
    req <- liftIO $ parseUrl url
    res <- liftIO $ requestPipe mgr req
    case statusCode (responseStatus res) of
        200 -> do
            liftIO $ putStrLn "status 200..."
            setHeader "Content-Type" "audio/mpeg"
            range <- header "Range"
            when (isJust range) $
                Spock.stream $ \write flush ->
                    runSafeT $ runEffect $
                        for (responseBody res) $ \chunk -> liftIO $ do
                            write $ BB.byteString chunk
                            flush
        _ -> do
            liftIO $ putStrLn "Whooops!"

writeFilePipe :: FilePath -> Consumer' ByteString (SafeT IO) r
writeFilePipe file = Pipes.withFile file WriteMode PipesBS.toHandle

data AuthConf = AuthConf
  { vkLogin    :: String
  , vkPassword :: String }

readAuthConf :: FilePath -> IO AuthConf
readAuthConf path =
    withFile path ReadMode $ \h -> do
        AuthConf <$> hGetLine h
                 <*> hGetLine h

tlsSettings :: Warp.TLSSettings
tlsSettings = Warp.defaultTlsSettings
    { Warp.certFile = "cert.pem"
    , Warp.keyFile  = "key.pem" }

main :: IO ()
main = do
    AuthConf{..} <- readAuthConf "./auth.conf"
    redisConn <- Redis.connect Redis.defaultConnectInfo
    withManager defaultManagerSettings $ \connMgr -> do
        app <- spockAsApp $ spockT runSafeT $ do
            get root $
                file "text/html" "./web/search.html"
            get ("api/audio" {-<//> var-}) $ do -- $ \audioId -> do
                --url <- liftIO $ runRedis redisConn $ Redis.get (BS.pack audioId)
                url <- param' "url"
                liftIO $ putStrLn "serving audio..."
                streamAudio connMgr url
            get "api/search" $ do
                query <- param' "query"
                entries <- liftIO $ Vk.exec vkLogin vkPassword (Vk.searchAudio query)
                json $ map Vk.musicRecordJson entries
        Warp.runTLS tlsSettings Warp.defaultSettings $
            Wai.staticPolicy (Wai.addBase "./web") app
