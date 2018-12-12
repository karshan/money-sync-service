{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import MoneySyncService.Types -- (GetDBResponse)
import Language.PureScript.Bridge (writePSTypes, buildBridge, defaultBridge, mkSumType)
import           Data.Time.Calendar            (Day (..))
import Data.Proxy (Proxy(..))
import GHC.Generics

deriving instance Generic Day

main :: IO ()
main = writePSTypes "frontend/src" (buildBridge defaultBridge) myTypes
  where
      myTypes = [ 
            mkSumType (Proxy :: Proxy GetDBResponse)
          , mkSumType (Proxy :: Proxy InstitutionId)
          , mkSumType (Proxy :: Proxy InstitutionResponse)
          , mkSumType (Proxy :: Proxy Account)
          , mkSumType (Proxy :: Proxy AccountType)
          , mkSumType (Proxy :: Proxy Balance)
          , mkSumType (Proxy :: Proxy Txn)
          , mkSumType (Proxy :: Proxy Day)
          ]
