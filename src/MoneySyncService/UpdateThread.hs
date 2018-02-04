{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module MoneySyncService.UpdateThread
    (updateThread) where

import           Control.Lens
import qualified Data.Map                        as Map
import qualified Lenses                          as L
import           MoneySyncService.DB
import qualified MoneySyncService.Scrapers.Chase as Chase
import           MoneySyncService.Types
import           Protolude

minuteDelay :: MonadIO m => Int -> m ()
minuteDelay n = liftIO $ threadDelay (n * 60 * 1000000)

updateThread :: (MonadReader DBHandle m, MonadIO m) => m ()
updateThread = do
    -- Get institutions from DB, and pass creds to scrape
    is <- Map.elems <$> getInstDB
    mapM_
        (\i -> do
            case i ^. L.creds of
                BofaCreds _ _ _ -> undefined
                ChaseCreds chaseReq -> do
                    eResult <- Chase.scrape chaseReq
                    either
                        addErrorLog
                        (merge (i ^. L.id))
                        eResult)
        is
    minuteDelay 60
    updateThread
