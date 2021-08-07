module Pact.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Pact.Web.Server.Constants
import Yesod.EmbeddedStatic

mkStatic :: Q [Dec]
mkStatic =
  mkEmbeddedStatic
    development
    "pactWebServerStatic"
    [embedDir "assets"]
