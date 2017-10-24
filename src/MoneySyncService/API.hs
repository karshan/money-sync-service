{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module MoneySyncService.API
    ( app
    ) where

import           MoneySyncService.DB    (DBHandle, DBHandler, addUser, getUsers)
import           MoneySyncService.Types (User)
import           Protolude
import           Servant

type API = "users" :> Get '[JSON] [User]
      :<|> "add"   :> ReqBody '[JSON] User :> Post '[JSON] ()

api :: Proxy API
api = Proxy

server :: DBHandle -> Server API
server acid = enter (runReaderTNat acid :: DBHandler :~> Handler) readerServerT

readerServerT :: ServerT API DBHandler
readerServerT = getUsers :<|> addUser

app :: DBHandle -> Application
app acid = serve api (server acid)
