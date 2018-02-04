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

type API = "db" :> "get" :> Get '[JSON] GetDBResponse
      :<|> "institution" :> "create" :> ReqBody '[JSON] CreateInstitution :> Post '[JSON] ()
      :<|> "errorlog" :> "get" :> Get '[JSON] [Text]

api :: Proxy API
api = Proxy

server :: DBHandle -> Server API
server acid = enter (runReaderTNat acid :: DBHandler :~> Handler) readerServerT

readerServerT :: ServerT API DBHandler
readerServerT = getDB :<|> addInst :<|> getErrorLog

app :: DBHandle -> Application
app acid = serve api (server acid)
