{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MoneySyncService
    ( startApp
    , testApp
    , defaultAppConfig
    ) where

import           MoneySyncService.API          (app)
import           MoneySyncService.DB           (openDB)
import           MoneySyncService.Types
import           MoneySyncService.UpdateThread (update, updateThread)
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

testApp :: AppConfig -> IO ()
testApp AppConfig{..} = do
    acid <- openDB dbDir
    either
        (\(e :: SomeException) -> putText $ "ERROR: " <> show e)
        (\ws -> do
            void $ forkIO (testScrape ws notificationConfig acid)
            putText $ "Listening on " <> show host <> ":" <> show port
            runSettings
                (defaultSettings &
                    setPort port &
                    setHost host)
                (app acid))
        =<< webhookServer host wsPort
    where
        testScrape ws notificationConfig acid = do
            putStrLn ("Press Enter" :: Text)
            _ <- getLine
            void (runReaderT (update ws notificationConfig) acid)
            testScrape ws notificationConfig acid


