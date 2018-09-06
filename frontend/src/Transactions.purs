module Transactions where

import Prelude
import Data.Maybe (fromMaybe, Maybe(..))
import Halogen as H
import Halogen.HTML as HH

type State = { ts :: Maybe String }

data Query a
  = Load String a

transactionsUI :: forall m. H.Component HH.HTML Query Unit Unit m
transactionsUI =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { ts: Nothing }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ ]
      [ HH.text (fromMaybe "Loading..." state.ts) ]

  eval :: Query ~> H.ComponentDSL State Query Unit m
  eval = case _ of
    Load ts_ next -> do
      H.put { ts: Just ts_ }
      pure next
