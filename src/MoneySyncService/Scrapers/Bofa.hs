{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module MoneySyncService.Scrapers.Bofa where

import MoneySyncService.Types
import MoneySyncService.Scrapers.API
import qualified Data.Aeson as Aeson
import qualified Lenses as L
import Control.Lens
import Protolude
import Data.Csv (decodeByName, FromNamedRecord)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import           Data.Time                     (Day, UTCTime (..))
import           Data.Time.Format              (defaultTimeLocale, parseTimeM)
import Util (dblUsd)

parseDate :: Text -> Either Text Day
parseDate dateS =
    maybeToEither ("parseDate \"" <> dateS <> "\" failed") $
        fmap utctDay $
        parseTimeM True defaultTimeLocale "%m/%d/%Y" $ toS dateS

parseDebitTxn :: BofaDebitCsv -> Either Text TxnRaw
parseDebitTxn t = do
    dt <- parseDate (t ^. L.date)
    amt <- maybeToEither ("Failed to parse amount " <> (t ^. L.amount)) $
                readMaybe $ toS (t ^. L.amount)
    return (emptyTxnRaw &
        L.name .~ t ^. L.description &
        L.date .~ dt &
        L.amount .~ dblUsd amt)

parseCreditTxn :: BofaCreditCsv -> Either Text TxnRaw
parseCreditTxn t = do
    dt <- parseDate (t ^. L.postedDate)
    amt <- maybeToEither ("Failed to parse amount " <> (t ^. L.amount)) $
                readMaybe $ toS (t ^. L.amount)
    return (emptyTxnRaw &
        L.name .~ t ^. L.payee &
        L.date .~ dt &
        L.amount .~ dblUsd amt)

goTxns :: FromNamedRecord a => (a -> Either Text TxnRaw) -> Text -> Either Text (Seq TxnRaw)
goTxns txnParser csvText = do
    (_, ts) <- over _Left toS . decodeByName . toS $ csvText
    foldl
        (\eOut t -> do
            out <- eOut
            parsedT <- txnParser t
            return $ out Seq.|> parsedT)
        (Right Seq.empty)
        ts

parseBalance :: Text -> Either Text Int
parseBalance b = maybeToEither ("Failed to parse balance " <> b) . fmap dblUsd . readMaybe .
    toS . T.filter (\c -> c /= '$' && c /= ',') $ b

goAccs :: [BofaDownloadedData] -> Either Text [MergeAccount]
goAccs =
    foldl
        (\eOut d -> do
            out <- eOut
            if d ^. L._type /= "CREDIT" && d ^. L._type /= "DEBIT" then
                Left $ "Invalid type " <> d ^. L._type
            else do
                let isDebit = d ^. L._type == "DEBIT"
                -- Debit CSV file has some non csv metadata on the top 6 lines which must be dropped
                txnRaws <- bool (goTxns parseCreditTxn) (goTxns parseDebitTxn) isDebit $
                                bool identity (T.unlines . drop 6 . T.lines) isDebit $ d ^. L.csv
                bal <- parseBalance $ d ^. L.balance
                Right $ (emptyMergeAccount &
                    L.balance .~ bal &
                    L._type .~ bool Credit Debit isDebit &
                    L.number .~ d ^. L.name & -- TODO parse name and get number
                    L.name .~ d ^. L.name &
                    L._3pLink .~ d ^. L.accountId &
                    L.txns .~ txnRaws):out)
        (Right [])

scrape :: MonadIO m => BofaCreds -> m (Either Text [MergeAccount])
scrape bofaCreds = do
    eResp <- run (scrapeBofa bofaCreds)
    either
        (return . Left . show)
        (\resp -> do
            (resp ^. L.downloadedData) &
                maybe
                    (return $ Left $ toS $ Aeson.encode $ resp ^. L.log)
                    (return . goAccs))
        eResp
