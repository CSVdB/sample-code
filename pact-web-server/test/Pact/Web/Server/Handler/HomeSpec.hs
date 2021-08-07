module Pact.Web.Server.Handler.HomeSpec (spec) where

import Pact.Web.Server.Handler.TestImport

spec :: Spec
spec = pactWebServerSpec $
  describe "HomeR" $
    yit "GETs a 200" $ do
      get HomeR
      statusIs 200
