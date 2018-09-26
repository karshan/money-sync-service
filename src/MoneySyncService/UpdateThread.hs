{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module MoneySyncService.UpdateThread
    (updateThread) where

import           Control.Lens
import qualified Data.Map                        as Map
import           Data.Time.Clock                 (getCurrentTime)
import           Data.Time.LocalTime             (getCurrentTimeZone,
                                                  utcToLocalTime)
import           Google.SendMail                 (sendMail')
import qualified Lenses                          as L
import           MoneySyncService.DB
import qualified MoneySyncService.Scrapers.Bofa  as Bofa
import qualified MoneySyncService.Scrapers.Chase as Chase
import           MoneySyncService.Types
import           Protolude
import           Text.Show.Pretty                (ppShow)
import           Webhook

minuteDelay :: MonadIO m => Int -> m ()
minuteDelay n = liftIO $ threadDelay (n * 60 * 1000000)

logMergeResults :: MonadIO m => NotificationConfig -> Text -> [(AccountId, [TxnRaw])] -> m ()
logMergeResults NotificationConfig{..} tag mergeResults = do
    tz <- liftIO getCurrentTimeZone
    curTime <- utcToLocalTime tz <$> liftIO getCurrentTime
    putStr $ "[" <> show curTime <> "]" <> " Merge Results <" <> tag <> ">: "
    let numNewTxns = length $ concatMap snd mergeResults
    if numNewTxns == 0 then
        putStrLn "No new txns"
    else do
        putStrLn ""
        sendResult <- liftIO $ sendMail' gsuiteKeyFile svcAccUser toEmail
            (tag <> " has " <> show numNewTxns <> " New Transactions")
            (ppShow mergeResults)
        either (\(e :: SomeException) -> liftIO $ putStrLn $ displayException e) (const $ return ()) sendResult
        print mergeResults

-- TODO MonadReader AppContext
updateThread :: (MonadReader DBHandle m, MonadIO m) => WebhookServer -> NotificationConfig -> m ()
updateThread ws c@NotificationConfig{..} = do
    -- Get institutions from DB, and pass creds to scrape
    is <- Map.elems <$> getInstDB
    acid <- ask
    mapM_
        (\i -> do
            case i ^. L.creds of
                BofaCredsT bofaReq -> do
                    wUrl <- liftIO $ webhook ws (\resp -> flip runReaderT acid $ do
                        either
                            (\e -> do
                                addErrorLog e
                                putStrLn "bofa scraper error"
                                sendResult <- liftIO $ sendMail' gsuiteKeyFile svcAccUser toEmail
                                    "money-sync-service bofa-scraper error" e
                                either (\(ex :: SomeException) -> liftIO $ putStrLn $ displayException ex) (const $ return ()) sendResult)
                            (\result -> logMergeResults c ("Bofa[" <> show (i ^. L.id) <> "]") =<<
                                            merge (i ^. L.id) result)
                            (Bofa.parse resp))
                    Bofa.scrape bofaReq wUrl
                ChaseCredsT chaseReq -> do
                    wUrl <- liftIO $ webhook ws (\resp -> flip runReaderT acid $ do
                        either
                            (\e -> do
                                addErrorLog e
                                putStrLn "chase scraper error"
                                sendResult <- liftIO $ sendMail' gsuiteKeyFile svcAccUser toEmail
                                    "money-sync-service chase-scraper error" e
                                either (\(ex :: SomeException) -> liftIO $ putStrLn $ displayException ex) (const $ return ()) sendResult)
                            (\result -> logMergeResults c ("Chase[" <> show (i ^. L.id) <> "]") =<<
                                            merge (i ^. L.id) result)
                            (Chase.parse resp))
                    Chase.scrape chaseReq wUrl)
        is
    minuteDelay (4 * 60)
    updateThread ws c
