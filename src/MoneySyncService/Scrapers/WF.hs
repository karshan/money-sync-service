{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Functions to parse WFResponse into Seq Txn
module MoneySyncService.Scrapers.WF where

import           Control.Lens
import qualified Data.Aeson                    as Aeson
import           Data.Csv                      (decode, HasHeader (NoHeader))
import           Data.Time                     (Day, UTCTime (..))
import           Data.Time.Format              (defaultTimeLocale, parseTimeM)
import qualified Lenses                        as L
import           MoneySyncService.Scrapers.API
import           MoneySyncService.Types
import           Protolude                     hiding (log, mask)
import           Util                          (dblUsd, parseBalance, parseDate)

goTxns :: Text -> Either Text [TxnRaw]
goTxns csv = do
    rs <- over _Left toS . decode NoHeader . toS $ csv
    foldl
        (\eOut (r :: WFCsv) -> do
            out <- eOut
            amt <- parseBalance $ r ^. L.amount
            dt <- parseDate $ r ^. L.date
            let t = emptyTxnRaw &
                    L.name .~ r ^. L.description &
                    L.amount .~ amt &
                    L.date .~ dt
            return $ t:out)
        (Right [])
        rs

goAccs :: [WFDownloadedData] -> Either Text [MergeAccount]
goAccs =
    foldl
        (\eOut d -> do
            out <- eOut
            if d ^. L._type /= "CREDIT" && d ^. L._type /= "DEBIT" then
                Left $ "Invalid type " <> d ^. L._type
            else do
                let isDebit = d ^. L._type == "DEBIT"
                txnRaws <- maybe (Right []) goTxns (d ^. L.csv)
                Right $ (emptyMergeAccount &
                    L.balance .~ dblUsd (d ^. L.balance) &
                    L._type .~ bool Credit Debit isDebit &
                    L.number .~ d ^. L.number &
                    L.name .~ d ^. L.name &
                    L._3pLink .~ d ^. L.number &
                    L.txns .~ txnRaws):out)
        (Right [])

parse :: ByteString -> Either Text [MergeAccount]
parse respString = do
    resp :: WFResponse <- over _Left toS $ Aeson.eitherDecode (toS respString)
    maybe
        (Left $ toS $ Aeson.encode $ resp ^. L.log)
        goAccs
        (resp ^. L.downloadedData)

scrape :: (MonadIO m) => WFCreds -> Text -> m ()
scrape wfCreds webhookUrl = do
    void $ run (scrapeWF (WFRequest wfCreds webhookUrl))
