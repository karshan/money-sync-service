{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Functions to parse ChaseResponse into Seq Txn
module MoneySyncService.Scrapers.Chase where

import           Control.Lens
import           Data.Aeson                    (encode)
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as S
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

goTxns :: [ChaseTransaction] -> Either Text (Seq TxnRaw)
goTxns cts =
    foldl
        (\eOut ct ->
            eOut >>= (\out -> do
                dt <- parseDate (ct ^. L.transactionDate)
                return $ out S.|> (emptyTxnRaw &
                    L.name .~ ct ^. L.description &
                    L.date .~ dt &
                    L.amount .~ dblUsd (ct ^. L.amount))))
        (Right S.empty)
        (S.fromList cts)

goAccs :: [ChaseAccountTile] -> Either Text [MergeAccount]
goAccs accTiles =
    foldl
        (\eOut accTile ->
            eOut >>= (\out ->
                if accTile ^. L.accountTileType /= "CARD" then
                    Left $ "Unknown cardType " <> accTile ^. L.cardType
                else do
                    txnRaws <- goTxns (accTile ^. L.transactions ^. L.result)
                    Right $ (emptyMergeAccount &
                        L.balance .~ dblUsd (accTile ^. L.tileDetail ^. L.currentBalance) &
                        L._type .~ Credit &
                        L.number .~ accTile ^. L.mask &
                        L.name .~ accTile ^. L.cardType &
                        L._3pLink .~ show (accTile ^. L.accountId) &
                        L.txns .~ txnRaws):out))
        (Right [])
        accTiles

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
