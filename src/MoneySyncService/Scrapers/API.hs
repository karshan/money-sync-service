{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
module MoneySyncService.Scrapers.API where

import           Data.Aeson              (Value)
import           Data.Aeson.TH           (Options (..), defaultOptions,
                                          deriveJSON)
import           Data.Proxy              (Proxy (..))
import           Network.HTTP.Client     (managerResponseTimeout, newManager,
                                          responseTimeoutMicro)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Protolude
import           Servant.API             ((:<|>) (..), (:>), JSON, Post,
                                          ReqBody)
import           Servant.Client          (BaseUrl (..), ClientEnv (..), ClientM,
                                          Scheme (Http), ServantError, client,
                                          runClientM)

-- TODO duplicaterecordfields + makeFieldsNoPrefix
data BofaRequest =
    BofaRequest {
        _username :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BofaRequest)

data BofaResponse =
    BofaResponse {
        _csv :: Maybe Text
      , _ok  :: Bool
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BofaResponse)

data ChaseCreds =
    ChaseCreds {
        _username :: Text
      , _password :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseCreds)

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
      , _result         :: [ChaseTransaction]
    } deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ChaseTransactions)

data ChaseTileDetail =
    ChaseTileDetail {
        _availableBalance :: Double
      , _currentBalance   :: Double
      , _lastPaymentDate  :: Text
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

type API = "bofa"  :> ReqBody '[JSON] BofaRequest :> Post '[JSON] BofaResponse
      :<|> "chase" :> ReqBody '[JSON] ChaseCreds :> Post '[JSON] ChaseResponse

scrapeBofa  :: BofaRequest  -> ClientM BofaResponse
scrapeChase :: ChaseCreds -> ClientM ChaseResponse
scrapeBofa :<|> scrapeChase = client (Proxy :: Proxy API)

run :: MonadIO m => ClientM resp -> m (Either ServantError resp)
run rpc = liftIO $ do
    mgr <- newManager (tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro (120 * 10^6) })
    runClientM rpc
        (ClientEnv mgr
            (BaseUrl Http "localhost" 8002 ""))
