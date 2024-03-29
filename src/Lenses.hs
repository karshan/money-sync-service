{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Lenses where

import           Control.Lens.TH               (makeFieldsNoPrefix)
import           MoneySyncService.Scrapers.API
import           MoneySyncService.Types

makeFieldsNoPrefix ''Txn
makeFieldsNoPrefix ''Balance
makeFieldsNoPrefix ''Account
makeFieldsNoPrefix ''Institution
makeFieldsNoPrefix ''TxnRaw
makeFieldsNoPrefix ''MergeAccount
makeFieldsNoPrefix ''CreateInstitution
makeFieldsNoPrefix ''UpdateInstitution
makeFieldsNoPrefix ''UpdateTags
makeFieldsNoPrefix ''InstitutionResponse
makeFieldsNoPrefix ''GetDBResponse

makeFieldsNoPrefix ''ChaseCreds
makeFieldsNoPrefix ''ChaseTransaction
makeFieldsNoPrefix ''ChaseTransactions
makeFieldsNoPrefix ''ChaseTileDetail
makeFieldsNoPrefix ''ChaseAccountTile
makeFieldsNoPrefix ''ChaseResponse
makeFieldsNoPrefix ''BofaCreds
makeFieldsNoPrefix ''BofaDownloadedData
makeFieldsNoPrefix ''BofaDebitCsv
makeFieldsNoPrefix ''BofaDebitBalanceCsv
makeFieldsNoPrefix ''BofaCreditCsv
makeFieldsNoPrefix ''BofaResponse
makeFieldsNoPrefix ''WFCreds
makeFieldsNoPrefix ''WFResponse
makeFieldsNoPrefix ''WFDownloadedData
makeFieldsNoPrefix ''WFCsv
