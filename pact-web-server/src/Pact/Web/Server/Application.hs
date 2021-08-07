{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Application where

import Pact.Web.Server.Foundation
import Pact.Web.Server.Handler

mkYesodDispatch "App" resourcesApp
