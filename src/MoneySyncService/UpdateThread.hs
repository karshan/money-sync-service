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
import           Google.SendMail                 (sendMail')
import qualified Lenses                          as L
import           MoneySyncService.DB
import qualified MoneySyncService.Scrapers.Bofa  as Bofa
import qualified MoneySyncService.Scrapers.Chase as Chase
import           MoneySyncService.Types
import           Protolude
import Data.Time.Clock (getCurrentTime)

minuteDelay :: MonadIO m => Int -> m ()
minuteDelay n = liftIO $ threadDelay (n * 60 * 1000000)

printMergeResults :: MonadIO m => Text -> [(AccountId, Int)] -> m ()
printMergeResults tag mergeResults = do
    curTime <- liftIO getCurrentTime
    putStrLn $ "[" <> show curTime <> "]" <> " Merge Results <" <> tag <> ">: "
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
                        (\result -> printMergeResults ("Bofa[" <> show (i ^. L.id) <> "]") =<< merge (i ^. L.id) result)
                        eResult
                ChaseCredsT chaseReq -> do
                    eResult <- Chase.scrape chaseReq
                    either
                        (\e -> do
                            liftIO $ sendMail' gsuiteKeyFile svcAccUser toEmail
                                "money-sync-service chase-scraper error" e
                            addErrorLog e)
                        (\result -> printMergeResults ("Chase[" <> show (i ^. L.id) <> "]") =<< merge (i ^. L.id) result)
                        eResult)
        is
    minuteDelay (4 * 60)
    updateThread c
