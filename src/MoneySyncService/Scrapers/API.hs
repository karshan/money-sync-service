{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
module MoneySyncService.Scrapers.API where

import           Data.Aeson              (Value)
import           Data.Aeson.TH           (Options (..), defaultOptions,
                                          deriveJSON)
import           Data.Csv                (DefaultOrdered (..), FromRecord (..),
                                          FromNamedRecord (..), header, (.:), (.!))
import           Data.Proxy              (Proxy (..))
import           Network.HTTP.Client     (managerResponseTimeout, newManager,
                                          responseTimeoutMicro)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Protolude
import           Servant.API             ((:<|>) (..), (:>), JSON, Post,
                                          ReqBody)
import           Servant.Client          (BaseUrl (..), mkClientEnv, ClientM,
                                          Scheme (Http), ServantError, client,
                                          runClientM)

data BofaDebitBalanceCsv =
    BofaDebitBalanceCsv {
        _description :: !Text
      , _empty       :: !Text
      , _summaryAmt  :: !Text
    } deriving (Eq, Show)

instance FromNamedRecord BofaDebitBalanceCsv where
    parseNamedRecord m = BofaDebitBalanceCsv <$> m .: "Description" <*> m .: "" <*> m .: "Summary Amt."

data BofaDebitCsv =
    BofaDebitCsv {
        _date        :: !Text
      , _description :: !Text
      , _amount      :: !Text
      , _runningBal  :: !Text
    } deriving (Eq, Show)

instance FromNamedRecord BofaDebitCsv where
    parseNamedRecord m = BofaDebitCsv <$> m .: "Date" <*> m .: "Description" <*> m .: "Amount" <*> m .: "Running Bal."

instance DefaultOrdered BofaDebitCsv where
    headerOrder _ = header ["Date", "Description", "Amount", "Running Bal."]

data BofaCreditCsv =
    BofaCreditCsv {
        _postedDate      :: !Text
      , _referenceNumber :: !Text
      , _payee           :: !Text
      , _address         :: !Text
      , _amount          :: !Text
    } deriving (Eq, Show)

instance FromNamedRecord BofaCreditCsv where
    parseNamedRecord m = BofaCreditCsv <$>
        m .: "Posted Date"      <*>
        m .: "Reference Number" <*>
        m .: "Payee" <*>
        m .: "Address" <*>
        m .: "Amount"

instance DefaultOrdered BofaCreditCsv where
    headerOrder _ = header ["Posted Date", "Reference Number", "Payee", "Address", "Amount"]

data BofaCreds =
    BofaCreds {
        _username              :: Text
      , _password              :: Text
      , _secretQuestionAnswers :: Map Text Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BofaCreds)

data BofaRequest =
    BofaRequest {
        _creds      :: BofaCreds
      , _webhookURL :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BofaRequest)

data BofaDownloadedData =
    BofaDownloadedData {
        _balance   :: Text
      , _accountId :: Text
      , _csvs      :: [Text]
      , _name      :: Text
      , __type     :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BofaDownloadedData)

data BofaResponse =
    BofaResponse {
        _downloadedData :: Maybe [BofaDownloadedData]
      , _log            :: Value
      , _ok             :: Bool
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BofaResponse)

data ChaseCreds =
    ChaseCreds {
        _username :: Text
      , _password :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseCreds)

data ChaseRequest =
    ChaseRequest {
        _creds      :: ChaseCreds
      , _webhookURL :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseRequest)

data ChaseTransaction =
    ChaseTransaction {
        _amount          :: Double
      , _description     :: Text
      , _pending         :: Bool
      , _transactionDate :: Text
      , _postDate        :: Maybe Text
      , _transactionId   :: Maybe Text
      -- TODO include 36 result.[n].type <- "R" = Return, "S" = Sale, "P" = Payment (of cc bill)
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseTransaction)

data ChaseTransactions =
    ChaseTransactions {
        _accountId      :: Int
      , _pendingCharges :: Double
      , _result         :: Maybe [ChaseTransaction]
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseTransactions)

data ChaseTileDetail =
    ChaseTileDetail {
        _availableBalance :: Double
      , _currentBalance   :: Double
      , _lastPaymentDate  :: Maybe Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseTileDetail)

data ChaseAccountTile =
    ChaseAccountTile {
        _accountId       :: Int
      , _tileDetail      :: ChaseTileDetail
      , _transactions    :: ChaseTransactions
      , _cardType        :: Text
      , _accountTileType :: Text
      , _mask            :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseAccountTile)

data ChaseResponse =
    ChaseResponse {
        _accountTiles :: Maybe [ChaseAccountTile]
      , _log          :: Value
      , _ok           :: Bool
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseResponse)

data WFCsv =
    WFCsv {
        _date        :: !Text
      , _amount      :: !Text
      , _description :: !Text
    } deriving (Eq, Show)
instance FromRecord WFCsv where
    parseRecord v
        | length v == 5 = WFCsv <$> v .! 0 <*> v .! 1 <*> v .! 4
        | otherwise = mzero


data WFDownloadedData =
    WFDownloadedData {
        _balance   :: Double
      , __type     :: Text
      , _csv       :: Maybe Text
      , _name      :: Text
      , _number    :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''WFDownloadedData)

data WFResponse =
    WFResponse {
        _downloadedData :: Maybe [WFDownloadedData]
      , _log            :: Value
      , _ok             :: Bool
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''WFResponse)

data WFCreds =
    WFCreds {
        _username :: Text
      , _password :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''WFCreds)

data WFRequest =
    WFRequest {
        _creds      :: WFCreds
      , _webhookURL :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''WFRequest)

type API = "bofa"  :> ReqBody '[JSON] BofaRequest :> Post '[JSON] ()
      :<|> "chase" :> ReqBody '[JSON] ChaseRequest :> Post '[JSON] ()
      :<|> "wf"    :> ReqBody '[JSON] WFRequest :> Post '[JSON] ()

scrapeBofa  :: BofaRequest -> ClientM ()
scrapeChase :: ChaseRequest -> ClientM ()
scrapeWF    :: WFRequest -> ClientM ()
scrapeBofa :<|> scrapeChase :<|> scrapeWF = client (Proxy :: Proxy API)

run :: MonadIO m => ClientM resp -> m (Either ServantError resp)
run rpc = liftIO $ do
    mgr <- newManager (tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro (120 * 10^6) })
    runClientM rpc
        (mkClientEnv mgr
            (BaseUrl Http "localhost" 3200 ""))
