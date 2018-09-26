{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module MoneySyncService.Scrapers.Bofa where

import           Control.Lens
import qualified Data.Aeson                    as Aeson
import           Data.Csv                      (FromNamedRecord, decodeByName)
import qualified Data.Text                     as T
import           Data.Time                     (Day, UTCTime (..))
import           Data.Time.Format              (defaultTimeLocale, parseTimeM)
import qualified Lenses                        as L
import           MoneySyncService.Scrapers.API
import           MoneySyncService.Types
import           Prelude                       ((!!))
import           Protolude
import           Util                          (dblUsd)

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
        L.amount .~ (negate $ dblUsd amt))

swapEither :: Either a b -> Either b a
swapEither (Left a)  = Right a
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
drop2nd []       = []
drop2nd (x:[])   = [x]
drop2nd (x:y:xs) = x:xs

assert :: Bool -> Text -> Either Text ()
assert False m = Left m
assert True _  = Right ()

parseDebitCsv :: Text -> Either Text ([TxnRaw], Int)
parseDebitCsv csv = do
    let debitCsvs = T.splitOn "\r\n\r\n" csv
    assert (length debitCsvs == 2) "Scraper.Bofa: length debitCsvs /= 2"
    let balanceCsv = debitCsvs !! 0
    let txnCsv = debitCsvs !! 1
    assert (length (T.lines txnCsv) > 1) "Scraper.Bofa: debitCsvText is empty"
    assert ("Beginning balance as of " `T.isInfixOf` ((T.lines txnCsv) !! 1))
        "Scraper.Bofa: First debit csv txn not \"Beginning balance as of...\""
    bal <- parseDebitBalance balanceCsv
    lTxnRaws <- goTxns parseDebitTxn $ T.unlines $ drop2nd $ T.lines txnCsv
    return (lTxnRaws, bal)

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
                (txnRaws, bal) <-
                    if isDebit then do
                        results <- mapM parseDebitCsv (d ^. L.csvs)
                        let lTxnRaws = concat $ map fst results
                        let bal = fromMaybe 0 $ head $ map snd results
                        return (lTxnRaws, bal)
                    else do
                        lTxnRaws <- concat <$> mapM (goTxns parseCreditTxn) (d ^. L.csvs)
                        bal <- parseBalance $ d ^. L.balance
                        return (lTxnRaws, bal)
                Right $ (emptyMergeAccount &
                    L.balance .~ bal &
                    L._type .~ bool Credit Debit isDebit &
                    L.number .~ T.reverse (T.take 4 (T.reverse (d ^. L.name))) &
                    L.name .~ d ^. L.name &
                    L._3pLink .~ d ^. L.accountId &
                    L.txns .~ txnRaws):out)
        (Right [])

parse :: ByteString -> Either Text [MergeAccount]
parse respString = do
    resp :: BofaResponse <- over _Left toS $ Aeson.eitherDecode (toS respString)
    maybe
        (Left $ toS $ Aeson.encode $ resp ^. L.log)
        goAccs
        (resp ^. L.downloadedData)

scrape :: MonadIO m => BofaCreds -> Text -> m ()
scrape bofaCreds webhookUrl = do
    void $ run (scrapeBofa (BofaRequest bofaCreds webhookUrl))
