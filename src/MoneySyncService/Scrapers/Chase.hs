{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Functions to parse ChaseResponse into Seq Txn
module MoneySyncService.Scrapers.Chase where

import           Control.Lens
import qualified Data.Aeson                    as Aeson
import           Data.Time                     (Day, UTCTime (..))
import           Data.Time.Format              (defaultTimeLocale, parseTimeM)
import qualified Lenses                        as L
import           MoneySyncService.Scrapers.API
import           MoneySyncService.Types
import           Protolude                     hiding (log, mask)
import           Util                          (dblUsd)

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
            if not $ ct ^. L.pending then do
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
                        txnRaws <- goTxns res
                        Right $ (emptyMergeAccount &
                            L.balance .~ dblUsd (accTile ^. (L.tileDetail . L.currentBalance)) &
                            L._type .~ Credit &
                            L.number .~ accTile ^. L.mask &
                            L.name .~ accTile ^. L.cardType &
                            L._3pLink .~ show (accTile ^. L.accountId) &
                            L.txns .~ txnRaws):out)
                    (accTile ^. (L.transactions . L.result)))
        (Right [])

parse :: ByteString -> Either Text [MergeAccount]
parse respString = do
    resp :: ChaseResponse <- over _Left toS $ Aeson.eitherDecode (toS respString)
    maybe
        (Left $ toS $ Aeson.encode $ resp ^. L.log)
        goAccs
        (resp ^. L.accountTiles)

scrape :: (MonadIO m) => ChaseCreds -> Text -> m ()
scrape chaseCreds webhookUrl = do
    void $ run (scrapeChase (ChaseRequest chaseCreds webhookUrl))
