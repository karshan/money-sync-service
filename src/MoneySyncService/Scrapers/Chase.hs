{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Functions to parse ChaseResponse into Seq Txn
module MoneySyncService.Scrapers.Chase where

import           Control.Lens
import           Data.Aeson                    (encode)
import           Data.Time                     (Day, UTCTime (..))
import           Data.Time.Format              (defaultTimeLocale, parseTimeM)
import qualified Lenses                        as L
import           MoneySyncService.Scrapers.API
import           MoneySyncService.Types
import           Protolude                     hiding (log, mask)
import           Util                          (dblUsd)
import Data.List.NonEmpty (nonEmpty)

parseDate :: Text -> Either Text Day
parseDate dateS =
    maybeToEither ("parseDate \"" <> dateS <> "\" failed") $
        fmap utctDay $
        parseTimeM True defaultTimeLocale "%Y%m%d" $ toS dateS

goTxns :: [ChaseTransaction] -> Either Text [TxnRaw]
goTxns =
    foldl
        (\eOut ct -> do
            out <- eOut
            -- Ignore pending transactions for now, these could be
            -- problematic if there amounts change for example
            if ct ^. L.pending == False then do
                dt <- parseDate (ct ^. L.transactionDate)
                return $ (emptyTxnRaw &
                    L.name .~ ct ^. L.description &
                    L.date .~ dt &
                    L.amount .~ dblUsd (ct ^. L.amount)):out
            else
                return out)
        (Right [])

goAccs :: [ChaseAccountTile] -> Either Text [MergeAccount]
goAccs =
    foldl
        (\eOut accTile -> do
            out <- eOut
            if accTile ^. L.accountTileType /= "CARD" then
                Left $ "Unknown cardType " <> accTile ^. L.cardType
            else
                maybe
                    (return out)
                    (\res -> do
                        lTxnRaws <- goTxns res
                        maybe
                            (return out) -- Don't actually think this is possible, chase would return result: [] (it usually just omits result entirely)
                            (\txnRaws ->
                                Right $ (emptyMergeAccount &
                                    L.balance .~ dblUsd (accTile ^. L.tileDetail ^. L.currentBalance) &
                                    L._type .~ Credit &
                                    L.number .~ accTile ^. L.mask &
                                    L.name .~ accTile ^. L.cardType &
                                    L._3pLink .~ show (accTile ^. L.accountId) &
                                    L.txns .~ txnRaws):out)
                            (nonEmpty lTxnRaws))
                    (accTile ^. L.transactions ^. L.result))
        (Right [])

-- TODO EitherT ?
scrape :: (MonadIO m) => ChaseCreds -> m (Either Text [MergeAccount])
scrape chaseCreds = do
    eResp <- run (scrapeChase chaseCreds)
    either
        (return . Left . show)
        (\resp -> do
            (resp ^. L.accountTiles) &
                maybe
                    (return $ Left $ toS $ encode $ resp ^. L.log)
                    (return . goAccs))
        eResp
