{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MoneySyncService.Scrapers.Bofa where

import MoneySyncService.Types
import MoneySyncService.Scrapers.API
import qualified Data.Aeson as Aeson
import qualified Lenses as L
import Control.Lens
import Protolude
import Data.Csv (decodeByName, FromNamedRecord)
import qualified Data.Text as T
import           Data.Time                     (Day, UTCTime (..))
import           Data.Time.Format              (defaultTimeLocale, parseTimeM)
import Util (dblUsd)
import Data.List.NonEmpty (nonEmpty)
import Prelude ((!!))

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

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right a) = Left a

parseDebitBalance :: Text -> Either Text Int
parseDebitBalance debitCsv = do
    (_, rs) <- over _Left toS . decodeByName . toS $ debitCsv
    swapEither $ foldl
        (\eOut (r :: BofaDebitBalanceCsv) -> do
            out <- eOut
            if "Ending balance as of " `T.isPrefixOf` (r ^. L.description) then
                maybe
                    (Right $ "failed to parse summaryAmt: " <> r ^. L.summaryAmt)
                    (Left . dblUsd)
                    (readMaybe $ toS (r ^. L.summaryAmt))
            else
                return out)
        (Right "parseDebitBalance failed")
        rs

goTxns :: FromNamedRecord a => (a -> Either Text b) -> Text -> Either Text [b]
goTxns txnParser csvText = do
    (_, ts) <- over _Left toS . decodeByName . toS $ csvText
    foldl
        (\eOut t -> do
            out <- eOut
            parsedT <- txnParser t
            return $ parsedT:out)
        (Right [])
        ts

parseBalance :: Text -> Either Text Int
parseBalance b = maybeToEither ("Failed to parse balance " <> b) . fmap dblUsd . readMaybe .
    toS . T.filter (\c -> c /= '$' && c /= ',') $ b

drop2nd :: [a] -> [a]
drop2nd [] = []
drop2nd (x:[]) = [x]
drop2nd (x:y:xs) = x:xs

assert :: Bool -> Text -> Either Text ()
assert False m = Left m
assert True _  = Right ()

goAccs :: [BofaDownloadedData] -> Either Text [MergeAccount]
goAccs =
    foldl
        (\eOut d -> do
            out <- eOut
            if d ^. L._type /= "CREDIT" && d ^. L._type /= "DEBIT" then
                Left $ "Invalid type " <> d ^. L._type
            else do
                let isDebit = d ^. L._type == "DEBIT"
                -- Debit CSV file has 2 csv's separated by a newline. The top csv is essentially useless
                -- also the first transaction is "Beginning balance as of..." which needs to be skipped
                (lTxnRaws, bal) <-
                    if isDebit then do
                        let debitCsvs = T.splitOn "\r\n\r\n" $ d ^. L.csv
                        assert (length debitCsvs == 2) "Scraper.Bofa: length debitCsvs /= 2"
                        let balanceCsv = debitCsvs !! 0
                        let txnCsv = debitCsvs !! 1
                        assert (length (T.lines txnCsv) > 1) "Scraper.Bofa: debitCsvText is empty"
                        assert ("Beginning balance as of " `T.isInfixOf` ((T.lines txnCsv) !! 1))
                            "Scraper.Bofa: First debit csv txn not \"Beginning balance as of...\""
                        lTxnRaws <- goTxns parseDebitTxn $ T.unlines $ drop2nd $ T.lines txnCsv
                        bal <- parseDebitBalance balanceCsv
                        return (lTxnRaws, bal)
                    else do
                        lTxnRaws <- goTxns parseCreditTxn (d ^. L.csv)
                        bal <- parseBalance $ d ^. L.balance
                        return (lTxnRaws, bal)
                maybe
                    (return out) -- TODO log empty txns ?
                    (\txnRaws ->
                        Right $ (emptyMergeAccount &
                            L.balance .~ bal &
                            L._type .~ bool Credit Debit isDebit &
                            L.number .~ T.reverse (T.take 4 (T.reverse (d ^. L.name))) &
                            L.name .~ d ^. L.name &
                            L._3pLink .~ d ^. L.accountId &
                            L.txns .~ txnRaws):out)
                    (nonEmpty lTxnRaws))
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
