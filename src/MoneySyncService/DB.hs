{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module MoneySyncService.DB
    ( DBHandle
    , DBHandler
    , getTxnDB
    , getAccountDB
    , getFcstTxnDB
    , openDB
    ) where

import           Control.Lens                  (over, view, (.~), (^.))
import           Control.Lens.TH               (makeLenses)
import           Crypto.Hash                   (SHA256 (SHA256), hashWith)
import           Data.Acid                     (AcidState, Query, Update,
                                                makeAcidic, openLocalStateFrom,
                                                query, update)
import           Data.ByteArray.Encoding       (Base (Base64), convertToBase)
import qualified Data.Map.Strict               as Map
import           Data.SafeCopy                 (base, deriveSafeCopy, safePut)
import           Data.Sequence                 (Seq, (|>))
import qualified Data.Sequence                 as S
import           Data.Serialize.Put            (runPut)
import qualified Data.Set                      as Set
import qualified Data.Text                     as T (take)
import           MoneySyncService.PseudoLenses (HasAccountIds (..))
import           MoneySyncService.Types
import           Protolude
import           Servant                       (Handler)

data Database =
    Database {
        _txnDB     :: Map TxnId Txn
      , _accountDB :: Map AccountId Account
      , _fcstTxnDB :: Map TxnId FcstTxn
    } deriving (Eq, Show)
makeLenses ''Database

instance HasAccountIds Database where
    accountIds = Set.fromList . Map.keys . view accountDB

emptyDB :: Database
emptyDB = Database Map.empty Map.empty Map.empty

getTxnDBEvt :: Query Database (Map TxnId Txn)
getTxnDBEvt = view txnDB <$> ask

getAccountDBEvt :: Query Database (Map AccountId Account)
getAccountDBEvt = view accountDB <$> ask

getFcstTxnDBEvt :: Query Database (Map TxnId FcstTxn)
getFcstTxnDBEvt = view fcstTxnDB <$> ask

$(deriveSafeCopy 0 'base ''Txn)
$(deriveSafeCopy 0 'base ''Balance)
$(deriveSafeCopy 0 'base ''AccountType)
$(deriveSafeCopy 0 'base ''Account)
$(deriveSafeCopy 0 'base ''FcstTxn)
$(deriveSafeCopy 0 'base ''Database)
$(makeAcidic ''Database [ 'getTxnDBEvt, 'getAccountDBEvt, 'getFcstTxnDBEvt ])

-- | Merges a set of new transactions into the db, assigning each txn a unique
-- id.
--
-- In theory it would be possible to use the same transaction id's given by
-- data providers (chase/bofa/plaid) but we don't.
--
-- The new transaction list can include transactions already in the db which
-- mustn't be duplicated. We use the following data to check for a duplicate
-- existing transactions:
--   txnName
--   txnDate
--   txnPostDate
--   txnAmount
--   txnAccount
-- We need to be careful when checking for duplicates, we cannot just check each
-- new transaction against the txnDB. Whenever we find a duplicate existing
-- transaction, it needs to be removed from the pool of transactions we check for
-- duplicates from. This must be done to avoid a case like the following:
--   The existing db contains a txn Chipotle,2017-01-01,$12.4
--   New transaction set contains 2 txn's Chipotle,2017-01-01,$12.4
-- If we checked each new txn against the txnDB, no new txn's would be added!
--
-- It is an error if a new txn refers to an accountId not in the db
--
-- TODO newTxns should have a type that indicates they don't have a txnId
mergeNewTxns :: Seq Txn     -- ^ set of new transactions.
             -> Update Database (Either Text ())
mergeNewTxns newTxns = do
    db <- get
    let foldResult =
            S.foldlWithIndex
                (\(eOutTxns, existingPool) i txn ->
                    case eOutTxns of
                        Right outTxns ->
                            if not $ (txn ^. txnAccount) `Set.member` accountIds db then
                                (Left ("txn refers to unknown accountId " <> show txn), existingPool)
                            else
                                maybe
                                    (do
                                        let newId = generateTxnId txn outTxns
                                        (Right $ Map.insert newId (txn & txnId .~ newId) outTxns, existingPool))
                                    (\existingDupeIndex ->
                                        (Right outTxns, S.deleteAt existingDupeIndex existingPool))
                                    (S.findIndexL
                                        (\existingT ->
                                            txn ^. txnName == existingT ^. txnName &&
                                            txn ^. txnDate == existingT ^. txnDate &&
                                            txn ^. txnPostDate == existingT ^. txnPostDate &&
                                            txn ^. txnAmount == existingT ^. txnAmount &&
                                            txn ^. txnAccount == existingT ^. txnAccount)
                                        existingPool)
                        Left err -> (Left err, existingPool))
                (Right $ db ^. txnDB, S.fromList $ Map.elems $ db ^. txnDB) -- TODO we can use a Map for existingPool also
                newTxns
    either
        (return . Left)
        (\newTxnMap -> fmap Right $ put (db & txnDB .~ newTxnMap))
        (fst foldResult)

-- TODO: rename update accounts, add docstring
mergeNewAccounts :: Map AccountId Account -> Update Database ()
mergeNewAccounts accountMap = modify (over accountDB (Map.union accountMap))

-- its impossible for undefined to be evaluated, the filter is on an infinite list
-- we could get rid of the undefined by rewriting this as explicit infinite recursion
generateTxnId :: Txn -> Map TxnId Txn -> TxnId
generateTxnId txn existingTxns =
    fromMaybe undefined $ head $ filter (\pId -> not (pId `Set.member` existingIds)) (map (T.take 8 . toS) possibleIds)
        where
            possibleIds = map (\(i, d) -> (convertToBase Base64 (hashWith SHA256 (d <> show i))) :: ByteString) $ zip [0..] $ repeat (runPut (safePut txn))
            existingIds = Map.keysSet existingTxns

query' a = liftIO . query a
update' a = liftIO . update a

type DBHandle = AcidState Database
type DBHandler = ReaderT DBHandle Handler

getTxnDB :: DBHandler (Map TxnId Txn)
getTxnDB = (`query'` GetTxnDBEvt) =<< ask

getAccountDB :: DBHandler (Map AccountId Account)
getAccountDB = (`query'` GetAccountDBEvt) =<< ask

getFcstTxnDB :: DBHandler (Map TxnId FcstTxn)
getFcstTxnDB = (`query'` GetFcstTxnDBEvt) =<< ask

--addUser :: User -> DBHandler ()
--addUser user = (`update'` AddUserEvt user) =<< ask

openDB :: FilePath -> IO DBHandle
openDB fp = openLocalStateFrom fp emptyDB
