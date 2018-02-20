{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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
        void $ liftIO $ sendMail' gsuiteKeyFile svcAccUser toEmail
            (tag <> " has " <> show numNewTxns <> " New Transactions")
            (ppShow mergeResults)
        print mergeResults

updateThread :: (MonadReader DBHandle m, MonadIO m) => NotificationConfig -> m ()
updateThread c@NotificationConfig{..} = do
    -- Get institutions from DB, and pass creds to scrape
    is <- Map.elems <$> getInstDB
    mapM_
        (\i -> do
            case i ^. L.creds of
                BofaCredsT bofaReq -> do
                    eResult <- Bofa.scrape bofaReq
                    either
                        (\e -> do
                            addErrorLog e
                            void $ liftIO $ sendMail' gsuiteKeyFile svcAccUser toEmail
                                "money-sync-service bofa-scraper error" e)
                        (\result -> logMergeResults c ("Bofa[" <> show (i ^. L.id) <> "]") =<< merge (i ^. L.id) result)
                        eResult
                ChaseCredsT chaseReq -> do
                    eResult <- Chase.scrape chaseReq
                    either
                        (\e -> do
                            liftIO $ sendMail' gsuiteKeyFile svcAccUser toEmail
                                "money-sync-service chase-scraper error" e
                            addErrorLog e)
                        (\result -> logMergeResults c ("Chase[" <> show (i ^. L.id) <> "]") =<< merge (i ^. L.id) result)
                        eResult)
        is
    minuteDelay (4 * 60)
    updateThread c
