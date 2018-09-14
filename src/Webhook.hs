{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Webhook
    ( webhookServer
    , webhook
    , WebhookServer
    ) where

import Protolude hiding (Map)
import           Network.Wai.Handler.Warp
import           Network.Wai
import Control.Concurrent.Map (Map)
import qualified Control.Concurrent.Map as Map
import           Network.HTTP.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import System.Random

data WebhookServer =
    WebhookServer { 
        wsMap  :: Map [Text] (ByteString -> IO ())
      , wsHost :: HostPreference
      , wsPort :: Port
    }

-- TODO pass f to callback so callback can respond to webhook ?
webhookServer :: Exception e => HostPreference -> Port -> IO (Either e WebhookServer)
webhookServer host port = do
    let app m req f = do
            maybe (putText $ "Unexpected webhook call to: " <> show (pathInfo req))
                (\callback -> do
                    reqBody <- strictRequestBody req
                    callback (toS reqBody :: BS.ByteString)
                    void $ Map.delete (pathInfo req) m)
                =<< Map.lookup (pathInfo req) m
            f (responseLBS status200 [(hContentType, "application/json")] "{}")
    try $ do
        m <- Map.empty
        forkIO $ runSettings (setPort port $ setHost "127.0.0.1" defaultSettings) (app m)
        return $ WebhookServer m host port

webhook :: WebhookServer -> (ByteString -> IO ()) -> IO Text
webhook ws callback = do
    webhookPath <- toS . B64.encode . BS.pack <$> replicateM 8 randomIO
    success <- Map.insertIfAbsent [webhookPath] callback $ wsMap ws
    if success then do
        return $ "http://" <> "localhost:" <> show (wsPort ws) <> "/" <> webhookPath
    else do
        putText "Webhook Url Gerenated already exists. Recursing"
        webhook ws callback
