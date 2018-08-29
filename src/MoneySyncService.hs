{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MoneySyncService
    ( startApp
    , defaultAppConfig
    ) where

import           MoneySyncService.API          (app)
import           MoneySyncService.DB           (openDB)
import           MoneySyncService.Types
import           MoneySyncService.UpdateThread (updateThread)
import           Network.Wai.Handler.Warp      (defaultSettings, runSettings,
                                                setHost, setPort)
import           Protolude
import           Webhook

startApp :: AppConfig -> IO ()
startApp AppConfig{..} = do
    acid <- openDB dbDir
    either
        (\(e :: SomeException) -> putText $ "ERROR: " <> show e)
        (\ws -> do
            void $ forkIO (runReaderT (updateThread ws notificationConfig) acid)
            putText $ "Listening on " <> show host <> ":" <> show port
            runSettings
                (defaultSettings &
                    setPort port &
                    setHost host)
                (app acid))
        =<< webhookServer host wsPort
