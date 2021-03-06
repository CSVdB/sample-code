{-# LANGUAGE TemplateHaskell #-}

module Pact.Web.Server.Constants where

import Language.Haskell.TH
import System.Environment

development :: Bool
development =
  $( do
       md <- runIO $ lookupEnv "DEVELOPMENT"
       fmap ConE $ case md of
         Nothing -> pure 'False
         Just _ -> do
           runIO $ putStrLn "WARNING: BUILDING IN DEVELOPMENT MODE"
           pure 'True
   )
