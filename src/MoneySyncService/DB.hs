{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module MoneySyncService.DB
    ( DBHandle
    , DBHandler
    , getUsers
    , addUser
    , openDB
    ) where

import           Control.Lens           (over, view)
import           Control.Lens.TH        (makeLenses)
import           Data.Acid              (AcidState, Query, Update, makeAcidic,
                                         openLocalStateFrom, query, update)
import           Data.SafeCopy          (base, deriveSafeCopy)
import           Protolude
import           Servant                (Handler)

import           MoneySyncService.Types

data Database = Database { _users :: [User] } deriving (Eq, Show)
makeLenses ''Database

getUsersEvt :: Query Database [User]
getUsersEvt = view users <$> ask

addUserEvt :: User -> Update Database ()
addUserEvt user = modify (over users (user:))

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Database)
$(makeAcidic ''Database [ 'addUserEvt, 'getUsersEvt ])

query' a = liftIO . query a
update' a = liftIO . update a

type DBHandle = AcidState Database
type DBHandler = ReaderT DBHandle Handler

getUsers :: DBHandler [User]
getUsers = (`query'` GetUsersEvt) =<< ask

addUser :: User -> DBHandler ()
addUser user = (`update'` AddUserEvt user) =<< ask

openDB :: FilePath -> IO DBHandle
openDB fp = openLocalStateFrom fp (Database [])
