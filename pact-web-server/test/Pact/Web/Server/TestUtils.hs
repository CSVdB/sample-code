{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Web.Server.TestUtils where

import Control.Lens ((&), (.~))
import Control.Monad.Logger
import Data.Text (Text)
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, runMigration, runSqlPool, walEnabled, withSqlitePoolInfo)
import Network.HTTP.Client as HTTP
import Pact.DB
import Pact.Data
import Pact.Web.Server.Application ()
import Pact.Web.Server.Foundation
import Pact.Web.Server.Gen
import Pact.Web.Server.Static
import Path.IO
import Test.Syd
import Test.Syd.Path
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod
import Yesod.Auth

type PactWebServerSpec = YesodSpec App

pactWebServerSpec :: PactWebServerSpec -> Spec
pactWebServerSpec = modifyMaxSuccess (`div` 20) . managerSpec . yesodSpecWithSiteSetupFunc serverSetup

serverSetup :: HTTP.Manager -> SetupFunc App
serverSetup man = do
  tdir <- tempDirSetupFunc "pact"
  pool <- pactConnectionPoolSetupFunc
  sessionKeyFile <- resolveFile tdir "session-key.aes"
  pure
    App
      { appLogLevel = LevelWarn,
        appStatic = pactWebServerStatic,
        appHTTPManager = man,
        appConnectionPool = pool,
        appSessionKeyFile = sessionKeyFile,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }

pactConnectionPoolSetupFunc :: SetupFunc DB.ConnectionPool
pactConnectionPoolSetupFunc = SetupFunc $ \func ->
  runNoLoggingT . withSqlitePoolInfo info 1 $ \pool -> do
    runSqlPool (runMigration serverMigration) pool
    liftIO $ func pool
  where
    info = mkSqliteConnectionInfo ":memory:" & walEnabled .~ False & fkEnabled .~ False

testRegisterUser :: TestUser -> YesodExample App ()
testRegisterUser TestUser {..} = testRegister testUsername testUserPassword

testRegister :: Username -> Text -> YesodExample App ()
testRegister username password = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addToken
    addPostParam "username" $ usernameText username
    addPostParam "password" password
    addPostParam "confirmPassword" password
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

testRegisterFail :: Username -> Text -> Text -> YesodExample App ()
testRegisterFail username password confirmPassword = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addToken
    addPostParam "username" $ usernameText username
    addPostParam "password" password
    addPostParam "confirmPassword" confirmPassword
  statusIs 303
  locationShouldBe $ AuthR registerR -- Failed to register
  _ <- followRedirect
  statusIs 200

testLogout :: YesodExample App ()
testLogout = do
  post $ AuthR LogoutR
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

testLoginUser :: TestUser -> YesodExample App ()
testLoginUser TestUser {..} = testLogin testUsername testUserPassword

testLogin :: Username -> Text -> YesodExample App ()
testLogin username password = do
  get $ AuthR LoginR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR loginR
    addToken
    addPostParam "username" $ usernameText username
    addPostParam "password" password
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

testLoginFailed :: Username -> Text -> YesodExample App ()
testLoginFailed username password = do
  get $ AuthR LoginR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR loginR
    addToken
    addPostParam "username" $ usernameText username
    addPostParam "password" password
  statusIs 303
  locationShouldBe $ AuthR LoginR -- Failed to log in
  _ <- followRedirect
  statusIs 200
