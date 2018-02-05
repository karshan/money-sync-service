{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
module MoneySyncService.Types where
-- TODO don't expose *Id constructors

import           Control.Lens.TH               (makeLenses)
import           Data.Aeson                    (FromJSONKey, ToJSONKey)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as S
import qualified Data.Set                      as Set
import           Data.Time.Calendar            (Day (..))
import           GHC.Generics                  (Generic)
import           MoneySyncService.Scrapers.API
import           Network.Wai.Handler.Warp      (HostPreference)
import           Protolude

data NotificationConfig =
    NotificationConfig {
        gsuiteKeyFile :: FilePath
      , svcAccUser    :: Text
      , toEmail       :: Text
    } deriving (Eq, Show)

data AppConfig =
    AppConfig {
        dbDir              :: FilePath
      , host               :: HostPreference
      , port               :: Int
      , notificationConfig :: NotificationConfig
    } deriving (Eq, Show)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig "db" "127.0.0.1" 8001 (NotificationConfig "svc-acc-key.json" "" "")

data InstitutionId = InstitutionId Text deriving (Eq, Ord, Show, Generic)

instance ToJSONKey InstitutionId
instance FromJSONKey InstitutionId

$(deriveJSON defaultOptions ''InstitutionId)

instance (StringConv InstitutionId Text) where
    strConv _ (InstitutionId t) = t

type AccountId = Text
type TxnId = Text
type Tag = Text -- TODO CI Text

data AccountType = Debit | Credit deriving (Eq, Show)
$(deriveJSON defaultOptions ''AccountType)

data Txn =
    Txn {
        _id        :: TxnId
      , _name      :: Text
      , _date      :: Day
      , _amount    :: Int
      , _accountId :: AccountId
      , _meta      :: Map Text Text
      , _tags      :: Set Tag
    } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Txn)

emptyTxn :: Txn
emptyTxn = Txn "" "" (ModifiedJulianDay 0) 0 "" Map.empty Set.empty

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
        _amount :: Int

        -- the DB api doesn't allow deleting a Txn without
        -- also deleting the corresponding account. So TxnId
        -- references are guaranteed to resolve.
      , _txnIds :: Set TxnId
    } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Balance)

emptyBalance :: Balance
emptyBalance = Balance 0 Set.empty

type Username = Text
type Password = Text
type SecretAnswers = [(Text, Text)]

data Creds =
    BofaCreds Username Password SecretAnswers
  | ChaseCreds ChaseCreds
        deriving (Eq)
$(deriveJSON defaultOptions ''Creds)

-- An instution has mutliple accounts.
-- e.g. you may have 2 credit cards (accounts) with Chase (institution)
data Account =
    Account {
        _id            :: AccountId
      , _balances      :: [Balance]
      , __type         :: AccountType
      , _number        :: Text
      , _name          :: Text
      , _institutionId :: InstitutionId

      -- We need a way to link scraped data from an instution to
      -- an account, this is it
      , __3pLink       :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Account)

emptyAccount :: Account
emptyAccount = Account "" [] Credit "" "" (InstitutionId "") ""

data Institution =
    Institution {
        _id    :: InstitutionId
      , _name  :: Text
      , _creds :: Creds
    } deriving (Eq)

emptyInstitution :: Institution
emptyInstitution = Institution (InstitutionId "") "" (BofaCreds "" "" [])

-- Txn without Ids
-- another way to structure these types could be to have Txn contain a TxnRaw
-- but tis not the case heer
data TxnRaw =
    TxnRaw {
        _name   :: Text
      , _date   :: Day
      , _amount :: Int
      , _meta   :: Map Text Text
      , _tags   :: Set Tag
    } deriving (Eq, Show)

emptyTxnRaw :: TxnRaw
emptyTxnRaw = TxnRaw "" (ModifiedJulianDay 0) 0 Map.empty Set.empty

data MergeAccount =
    MergeAccount {
        _balance :: Int
      , __type   :: AccountType
      , _number  :: Text
      , _name    :: Text
      , __3pLink :: Text
      , _txns    :: Seq TxnRaw
    } deriving (Eq, Show)

emptyMergeAccount :: MergeAccount
emptyMergeAccount = MergeAccount 0 Credit "" "" "" S.empty

data CreateInstitution =
    CreateInstitution {
        _name  :: Text
      , _creds :: Creds
    } deriving (Eq)
$(deriveJSON defaultOptions ''CreateInstitution)

data InstitutionResponse =
    InstitutionResponse {
        _id   :: InstitutionId
      , _name :: Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions ''InstitutionResponse)

emptyInstitutionResponse :: InstitutionResponse
emptyInstitutionResponse = InstitutionResponse (InstitutionId "") ""

data GetDBResponse =
    GetDBResponse {
        _institutions :: Map InstitutionId InstitutionResponse
      , _accounts     :: Map AccountId Account
      , _txns         :: Map TxnId Txn
    } deriving (Eq, Show)
$(deriveJSON defaultOptions ''GetDBResponse)

emptyGetDBResponse :: GetDBResponse
emptyGetDBResponse = GetDBResponse Map.empty Map.empty Map.empty
