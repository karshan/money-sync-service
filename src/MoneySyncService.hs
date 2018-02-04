{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

startApp :: AppConfig -> IO ()
startApp AppConfig{..} = do
    acid <- openDB dbDir
    void $ forkIO (runReaderT (updateThread notificationConfig) acid)
    putText $ "Listening on " <> show host <> ":" <> show port
    runSettings
        (defaultSettings &
            setPort port &
            setHost host)
        (app acid)
