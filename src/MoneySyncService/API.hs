{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module MoneySyncService.API where

import           MoneySyncService.DB
import           MoneySyncService.Types
import           Protolude
import           Servant
import           Prelude (String)

type API = "db" :> "get" :> Get '[JSON] GetDBResponse
      :<|> "institution" :> "create" :> ReqBody '[JSON] CreateInstitution :> Post '[JSON] ()
      :<|> "tags" :> "update" :> ReqBody '[JSON] UpdateTags :> Post '[JSON] ()
      :<|> "errorlog" :> "get" :> Get '[JSON] [Text]
      :<|> "errorlog" :> "clear" :> Post '[JSON] ()

type StaticAPI =
           API
      :<|> "static" :> Raw
      :<|> Verb 'GET 302 '[PlainText] (Headers '[Header "Location" String] NoContent)

api :: Proxy StaticAPI
api = Proxy

server :: DBHandle -> Server API
server acid = enter (runReaderTNat acid :: DBHandler :~> Handler) readerServerT

readerServerT :: ServerT API DBHandler
readerServerT = getDB :<|> addInst :<|> updateTags :<|> getErrorLog :<|> clearErrorLog

app :: DBHandle -> Application
app acid = serve api $
    server acid
        :<|> serveDirectoryFileServer "static/"
        :<|> return (addHeader "/static/index.html" NoContent)
