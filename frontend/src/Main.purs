module Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Transactions as TS
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import BaseUrl (baseUrl)
import Effect.Console (log)
import Effect.Class (liftEffect)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  ui <- runUI TS.transactionsUI unit body
  res <- AX.get ResponseFormat.json $ baseUrl <> "/db/get" 
  case res.body of
      Left err -> liftEffect $ log (AX.printResponseFormatError err) -- TODO send error to some other component
      Right json -> ui.query (TS.Load json unit)
  pure unit
