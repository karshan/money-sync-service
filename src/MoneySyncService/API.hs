{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module MoneySyncService.API
    ( app
    ) where

import           MoneySyncService.DB
import           MoneySyncService.Types
import           Protolude
import           Servant

type API = "txns" :> Get '[JSON] (Map TxnId Txn)
      :<|> "add"   :> ReqBody '[JSON] Txn :> Post '[JSON] ()

api :: Proxy API
api = Proxy

server :: DBHandle -> Server API
server acid = enter (runReaderTNat acid :: DBHandler :~> Handler) readerServerT

readerServerT :: ServerT API DBHandler
readerServerT = getTxnDB :<|> (\_ -> return ())

app :: DBHandle -> Application
app acid = serve api (server acid)
