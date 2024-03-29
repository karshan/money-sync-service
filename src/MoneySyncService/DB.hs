{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module MoneySyncService.DB where

import           Control.Lens                  (over, view, (.~), (^.))
import           Control.Lens.TH               (makeLenses)
import           Data.Acid                     (AcidState, Query, Update,
                                                makeAcidic, openLocalStateFrom,
                                                query, update)
import qualified Data.Map.Strict               as Map
import           Data.SafeCopy                 (base, deriveSafeCopy)
import qualified Data.Set                      as Set
import qualified Lenses                        as L
import           MoneySyncService.Scrapers.API
import           MoneySyncService.Types
import           Prelude                       (error)
import           Protolude
import           Servant                       (Handler)
import           System.Random                 (StdGen, mkStdGen)
import           Util                          (randomText, fuzzyMatch)

-- TODO is it possible to ensure with types that the DB can never contain
-- transactions that refer to a non-existent accountId ?
data Database =
    Database {
        _txnDB      :: Map TxnId Txn
      , _instDB     :: Map InstitutionId Institution
      , _accountDB  :: Map AccountId Account
      , _fcstTxnDB  :: Map TxnId FcstTxn
      , _tagRuleDB  :: Map TagRuleId TagRule
      , _errorLogDB :: [Text]
      , _rndSeed    :: StdGen
    }
makeLenses ''Database

emptyDB :: Database
emptyDB = Database Map.empty Map.empty Map.empty Map.empty Map.empty [] (mkStdGen 0)

getDBEvt :: Query Database GetDBResponse
getDBEvt =
    (\db -> emptyGetDBResponse &
        L.institutions .~ map (\a -> emptyInstitutionResponse &
                                L.id .~ a ^. L.id &
                                L.name .~ a ^. L.name)
                            (db ^. instDB) &
        L.accounts .~ db ^. accountDB &
        L.txns .~ db ^. txnDB) <$> ask

getTxnDBEvt :: Query Database (Map TxnId Txn)
getTxnDBEvt = view txnDB <$> ask

getAccountDBEvt :: Query Database (Map AccountId Account)
getAccountDBEvt = view accountDB <$> ask

getFcstTxnDBEvt :: Query Database (Map TxnId FcstTxn)
getFcstTxnDBEvt = view fcstTxnDB <$> ask

getInstDBEvt :: Query Database (Map InstitutionId Institution)
getInstDBEvt = view instDB <$> ask

getTagRuleDBEvt :: Query Database (Map TagRuleId TagRule)
getTagRuleDBEvt = view tagRuleDB <$> ask

getErrorLogEvt :: Query Database [Text]
getErrorLogEvt = view errorLogDB <$> ask

generateId :: Set Text -> Update Database Text
generateId existingIds = do
    _g <- view rndSeed <$> get
    go _g
        where
            go g = let (newId, nextG) = randomText g 10
                   in if not (newId `Set.member` existingIds) then do
                          modify (over rndSeed (const nextG))
                          return newId
                      else
                          go nextG

-- | Remove duplicate txns in given [TxnRaw]
--
-- We use the following data to check for a duplicate
-- existing transactions:
--   name
--   date
--   amount
--   accountId
-- We need to be careful when checking for duplicates, we cannot just check each
-- new transaction against the curTxns. Whenever we find a duplicate existing
-- transaction, it needs to be removed from the pool of transactions we check for
-- duplicates from. This must be done to avoid a case like the following:
--   The existing db contains a txn Chipotle,2017-01-01,$12.4
--   New transaction set contains 2 txn's Chipotle,2017-01-01,$12.4
-- If we checked each new txn against the txnDB, no new txn's would be added!
--
-- TODO rewrite using StateT existingPool (Either Text) ?
removeDupeTxns :: AccountId     -- ^ AccountId associated with newTxns
               -> Map TxnId Txn -- ^ current Txn database
               -> [TxnRaw]      -- ^ newTxns: remove duplicates in this list
               -> [TxnRaw]
removeDupeTxns accId curTxns newTxns =
    fst $ foldl
        (\(outTxns, existingPool) txn ->
            maybe
                (txn:outTxns, existingPool)
                (\existingDupe ->
                    (outTxns, Map.delete (existingDupe ^. L.id) existingPool))
                (head $ Map.filter
                    (\existingT ->
                        (txn ^. L.name) `fuzzyMatch` (existingT ^. L.name) &&
                        txn ^. L.date == existingT ^. L.date &&
                        txn ^. L.amount == existingT ^. L.amount &&
                        accId == existingT ^. L.accountId)
                    existingPool))
        ([], curTxns) -- TODO we can use a Map for existingPool also
        newTxns

-- TODO error if refer to non-existent account id ?
putTxn :: AccountId -> TxnRaw -> Update Database TxnId
putTxn accId txn = do
    existingTxnIds <- Map.keysSet . view txnDB <$> get
    newId <- generateId existingTxnIds
    modify (over txnDB (Map.insert newId (mkTxn accId newId txn)))
    return newId

matchingAccount :: InstitutionId -> Map AccountId Account -> MergeAccount -> Maybe Account
matchingAccount instId accDB mergeAcc =
    head $ Map.elems $ Map.filter
        (\acc ->
            acc ^. L.institutionId == instId &&
            acc ^. L._3pLink  == mergeAcc ^. L._3pLink) accDB

-- TODO error if non-existent institution id ?
-- TODO verify new balance against old balance here and report failures ?
putAccount :: InstitutionId -> MergeAccount
           -> Update Database (AccountId, [TxnRaw]) -- ^ (generated/existing id, merged TxnRaws)
putAccount instId mergeAcc = do
    mExistingAcc <- (\db -> matchingAccount instId (db ^. accountDB) mergeAcc) <$> get
    existingAccIds <- Map.keysSet . view accountDB <$> get
    accId <- maybe (generateId existingAccIds) return (view L.id <$> mExistingAcc)
    curTxns <- Map.filter ((== accId) . view L.accountId) . view txnDB <$> get
    let existingOldBals = fromMaybe [] (view L.oldBalances <$> mExistingAcc)
    let newTxns = removeDupeTxns accId curTxns (toList $ mergeAcc ^. L.txns)
    newTxnIds <- Set.fromList . toList <$> mapM (putTxn accId) newTxns
    let sortDesc a b = (flip compare `on` view L.date) a b <> (flip compare `on` view L.id) a b
    -- FIXME: there could be an account with 0 txns, fail gracefully
    mLatestTxnId <- map (view L.id) . head . sortBy sortDesc . toList .
            Map.filter ((== accId) . view L.accountId) . view txnDB <$> get
    if mLatestTxnId == Nothing then
        return (accId, [])
    else do
        let latestTxnId = fromMaybe (error "impossible") mLatestTxnId
        let mExistingCurBal = view L.balance <$> mExistingAcc
        let (newCurBal, newOldBals) =
                -- If the database contains newer txns than the txns in this merge
                -- don't update the current balance (the balance is old).
                if latestTxnId `Set.member` newTxnIds then
                    (emptyBalance &
                        L.amount .~ mergeAcc ^. L.balance &
                        L.txnId .~ latestTxnId
                      , maybe existingOldBals (:existingOldBals) mExistingCurBal)
                else
                    -- In theory we should add mergeAcc.balance to existingOldBals here
                    -- FIXME: there could be an account with 0 txns, fail gracefully
                    (fromMaybe (error "Account with 0 txns") mExistingCurBal, existingOldBals)
        modify (over accountDB (Map.insert accId (mkAccount instId accId newCurBal newOldBals mergeAcc)))
        return (accId, newTxns)

mkAccount :: InstitutionId -> AccountId -> Balance -> [Balance] -> MergeAccount -> Account
mkAccount instId accId curBal oldBals mAcc =
    emptyAccount &
        L.id .~ accId &
        L.balance .~ curBal &
        L.oldBalances .~ oldBals &
        L._type .~ mAcc ^. L._type &
        L.number .~ mAcc ^. L.number &
        L.name .~ mAcc ^. L.name &
        L.institutionId .~ instId &
        L._3pLink .~ mAcc ^. L._3pLink

mkTxn :: AccountId -> TxnId -> TxnRaw -> Txn
mkTxn accId tId txn =
    emptyTxn &
        L.id .~ tId &
        L.name .~ txn ^. L.name &
        L.date .~ txn ^. L.date &
        L.amount .~ txn ^. L.amount &
        L.accountId .~ accId &
        L.meta .~ txn ^. L.meta &
        L.tags .~ txn ^. L.tags

-- Returns number of merged txns
mergeEvt :: InstitutionId -> [MergeAccount] -> Update Database [(AccountId, [TxnRaw])]
mergeEvt gInstId =
    mapM (putAccount gInstId)

addInstEvt :: CreateInstitution -> Update Database ()
addInstEvt a = do
    existingIds <- Map.keysSet . Map.mapKeys toS . view instDB <$> get
    newId <- InstitutionId <$> generateId existingIds
    modify
        (over instDB $
                Map.insert
                    newId
                    (emptyInstitution &
                        L.id .~ newId &
                        L.name .~ a ^. L.name &
                        L.creds .~ a ^. L.creds))

-- delete all txns and accounts in the DB associated with the given institution
removeInstEvt :: InstitutionId -> Update Database ()
removeInstEvt instId = do
    -- accIds associated with given institution id
    accIds <- Map.keysSet . Map.filter ((== instId) . view L.institutionId) . view accountDB <$> get
    modify (over accountDB (Map.filterWithKey (\k _ -> k `Set.notMember` accIds)))
    modify (over txnDB (Map.filter (flip Set.notMember accIds . view L.accountId)))
    modify (over instDB (Map.delete instId))

-- for now only institution creds can be updated
updateInstEvt :: UpdateInstitution -> Update Database ()
updateInstEvt updateReq =
    modify (over instDB (Map.adjust (L.creds .~ (updateReq ^. L.creds)) (updateReq ^. L.id)))

evalTagOp :: TagOp -> Set Tag -> Set Tag
evalTagOp (AddTags newTags) origTags = Set.union newTags origTags
evalTagOp (RemoveTags tagsToRemove) origTags = origTags `Set.difference` tagsToRemove

updateTagsEvt :: UpdateTags -> Update Database ()
updateTagsEvt req =
    modify (over txnDB (Map.mapWithKey
        (\txnId txn ->
            if txnId `Set.member` (req ^. L.ids) then
                txn &
                    L.tags .~ evalTagOp (req ^. L.op) (txn ^. L.tags)
            else
                txn)))

addTagRuleEvt :: TagRule -> Update Database ()
addTagRuleEvt a = do
    existingIds <- Map.keysSet . Map.mapKeys toS . view tagRuleDB <$> get
    newId <- TagRuleId <$> generateId existingIds
    modify (over tagRuleDB (Map.insert newId a))

removeTagRuleEvt :: TagRuleId -> Update Database ()
removeTagRuleEvt tagRuleId = modify (over tagRuleDB (Map.delete tagRuleId))

addErrorLogEvt :: Text -> Update Database ()
addErrorLogEvt newLog = modify (over errorLogDB (newLog:))

clearErrorLogEvt :: Update Database ()
clearErrorLogEvt = modify (over errorLogDB (const []))

$(deriveSafeCopy 0 'base ''Txn)
$(deriveSafeCopy 0 'base ''Balance)
$(deriveSafeCopy 0 'base ''AccountType)
$(deriveSafeCopy 0 'base ''ChaseCreds)
$(deriveSafeCopy 0 'base ''BofaCreds)
$(deriveSafeCopy 0 'base ''WFCreds)
$(deriveSafeCopy 0 'base ''Creds)
$(deriveSafeCopy 0 'base ''InstitutionId)
$(deriveSafeCopy 0 'base ''Institution)
$(deriveSafeCopy 0 'base ''Account)
$(deriveSafeCopy 0 'base ''TxnRaw)
$(deriveSafeCopy 0 'base ''MergeAccount)
$(deriveSafeCopy 0 'base ''FcstTxn)
$(deriveSafeCopy 0 'base ''TagRuleId)
$(deriveSafeCopy 0 'base ''TagRule)
$(deriveSafeCopy 1 'base ''Database)
$(deriveSafeCopy 0 'base ''InstitutionResponse)
$(deriveSafeCopy 0 'base ''GetDBResponse)
$(deriveSafeCopy 0 'base ''CreateInstitution)
$(deriveSafeCopy 0 'base ''UpdateInstitution)
$(deriveSafeCopy 0 'base ''TagOp)
$(deriveSafeCopy 0 'base ''UpdateTags)
$(deriveSafeCopy 0 'base ''StdGen)
$(makeAcidic ''Database [ 'getTxnDBEvt
                        , 'getAccountDBEvt
                        , 'getFcstTxnDBEvt
                        , 'getInstDBEvt
                        , 'getDBEvt
                        , 'getTagRuleDBEvt
                        , 'getErrorLogEvt
                        , 'mergeEvt
                        , 'addInstEvt
                        , 'removeInstEvt
                        , 'addTagRuleEvt
                        , 'removeTagRuleEvt
                        , 'updateInstEvt
                        , 'updateTagsEvt
                        , 'addErrorLogEvt
                        , 'clearErrorLogEvt
                        ])

query' a = liftIO . query a
update' a = liftIO . update a

type DBHandle = AcidState Database
type DBHandler = ReaderT DBHandle Handler

getTxnDB :: (MonadReader DBHandle m, MonadIO m) => m (Map TxnId Txn)
getTxnDB = (`query'` GetTxnDBEvt) =<< ask

getAccountDB :: (MonadReader DBHandle m, MonadIO m) => m (Map AccountId Account)
getAccountDB = (`query'` GetAccountDBEvt) =<< ask

getFcstTxnDB :: (MonadReader DBHandle m, MonadIO m) => m (Map TxnId FcstTxn)
getFcstTxnDB = (`query'` GetFcstTxnDBEvt) =<< ask

getInstDB :: (MonadReader DBHandle m, MonadIO m) => m (Map InstitutionId Institution)
getInstDB = (`query'` GetInstDBEvt) =<< ask

getDB :: (MonadReader DBHandle m, MonadIO m) => m GetDBResponse
getDB = (`query'` GetDBEvt) =<< ask

getTagRuleDB :: (MonadReader DBHandle m, MonadIO m) => m (Map TagRuleId TagRule)
getTagRuleDB = (`query'` GetTagRuleDBEvt) =<< ask

getErrorLog :: (MonadReader DBHandle m, MonadIO m) => m [Text]
getErrorLog = (`query'` GetErrorLogEvt) =<< ask

merge :: (MonadReader DBHandle m, MonadIO m) => InstitutionId -> [MergeAccount] -> m [(AccountId, [TxnRaw])]
merge instId mergeAccs = (`update'` MergeEvt instId mergeAccs) =<< ask

addInst :: (MonadReader DBHandle m, MonadIO m) => CreateInstitution -> m ()
addInst institution = (`update'` AddInstEvt institution) =<< ask

removeInst :: (MonadReader DBHandle m, MonadIO m) => InstitutionId -> m ()
removeInst instId = (`update'` RemoveInstEvt instId) =<< ask

addTagRule :: (MonadReader DBHandle m, MonadIO m) => TagRule -> m ()
addTagRule tagRule = (`update'` AddTagRuleEvt tagRule) =<< ask

removeTagRule :: (MonadReader DBHandle m, MonadIO m) => TagRuleId -> m ()
removeTagRule tagRuleId = (`update'` RemoveTagRuleEvt tagRuleId) =<< ask

updateInst :: (MonadReader DBHandle m, MonadIO m) => UpdateInstitution -> m ()
updateInst updateReq = (`update'` UpdateInstEvt updateReq) =<< ask

updateTags :: (MonadReader DBHandle m, MonadIO m) => UpdateTags -> m ()
updateTags req = (`update'` UpdateTagsEvt req) =<< ask

addErrorLog :: (MonadReader DBHandle m, MonadIO m) => Text -> m ()
addErrorLog errorLog = (`update'` AddErrorLogEvt errorLog) =<< ask

clearErrorLog :: (MonadReader DBHandle m, MonadIO m) => m ()
clearErrorLog = (`update'` ClearErrorLogEvt) =<< ask

openDB :: FilePath -> IO DBHandle
openDB fp = do
    openLocalStateFrom fp emptyDB
