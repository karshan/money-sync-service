{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module MoneySyncService
    ( startApp
    , defaultAppConfig
    ) where

import           MoneySyncService.API          (app)
import           MoneySyncService.DB           (openDB)
import           MoneySyncService.UpdateThread (updateThread)
import           Network.Wai.Handler.Warp      (HostPreference, defaultSettings,
                                                runSettings, setHost, setPort)
import           Protolude

data AppConfig =
    AppConfig {
        dbDir          :: FilePath
      , host           :: HostPreference
      , port           :: Int
    } deriving (Eq, Show)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig "db" "127.0.0.1" 8080

startApp :: AppConfig -> IO ()
startApp AppConfig{..} = do
    acid <- openDB dbDir
    void $ forkIO (runReaderT updateThread acid)
    putText $ "Listening on " <> show host <> ":" <> show port
    runSettings
        (defaultSettings &
            setPort port &
            setHost host)
        (app acid)
