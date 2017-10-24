{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module MoneySyncService.Types
    ( User (..)
    ) where

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Protolude

data User = User
  { userId        :: Int
  , userFirstName :: Text
  , userLastName  :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
