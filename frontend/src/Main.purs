module Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Transactions as TS
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import BaseUrl (baseUrl)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  ui <- runUI TS.transactionsUI unit body
  res <- AX.get ResponseFormat.json $ baseUrl <> "/db/get" 
  case res.body of
      Left err -> ui.query (TS.Load (AX.printResponseFormatError err) unit)
      Right json -> ui.query (TS.Load (J.stringify json) unit)
  pure unit
