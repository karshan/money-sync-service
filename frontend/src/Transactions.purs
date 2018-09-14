module Transactions where

import Prelude
import Data.Maybe (fromMaybe, Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J

type State = { ts :: Maybe Json }

data Query a
  = Load Json a

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
      [ HH.text (fromMaybe "Loading..." $ map J.stringify state.ts) ]

  eval :: Query ~> H.ComponentDSL State Query Unit m
  eval = case _ of
    Load ts_ next -> do
      H.put { ts: Just ts_ }
      pure next
