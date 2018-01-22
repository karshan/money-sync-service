{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module MoneySyncService.Types where

import           Control.Lens.TH     (makeLenses)
import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.Time.Calendar  (Day)
import           Protolude

type AccountId = Text
type TxnId = Text
type Tag = Text

data AccountType = Debit | Credit deriving (Eq, Show)
$(deriveJSON defaultOptions ''AccountType)

data Txn =
    Txn {
        _txnId       :: TxnId
      , _txnName     :: Text
      , _txnDate     :: Day
      , _txnPostDate :: Day
      , _txnAmount   :: Int
      , _txnAccount  :: AccountId
      , _txnMeta     :: Map Text Text
      , _txnTags     :: Set Tag
    } deriving (Eq, Show)
makeLenses ''Txn
$(deriveJSON defaultOptions ''Txn)

-- Forecasted Txn
data FcstTxn =
    FcstTxn {
        _fcstTxn          :: Txn
      , _fcstTxnOccurance :: Maybe TxnId
    } deriving (Eq, Show)
makeLenses ''FcstTxn
$(deriveJSON defaultOptions ''FcstTxn)

data Balance =
    Balance {
        _balanceAmount :: Int
      , _balanceTxn    :: Txn
    } deriving (Eq, Show)
makeLenses ''Balance
$(deriveJSON defaultOptions ''Balance)

data Account =
    Account {
        _accountId       :: AccountId
      , _accountBalances :: [Balance]
      , _accountType     :: AccountType
      , _accountNumber   :: Text
      , _accountName     :: Text
    } deriving (Eq, Show)
makeLenses ''Account
$(deriveJSON defaultOptions ''Account)
