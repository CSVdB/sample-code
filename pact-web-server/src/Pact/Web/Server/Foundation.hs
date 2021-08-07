{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Web.Server.Foundation where

import Data.FileEmbed (makeRelativeToProject)
import Data.Text (Text)
import Database.Persist.Sql
import Network.HTTP.Client as HTTP
import Pact.DB
import Pact.Data
import Pact.Web.Server.Static
import Pact.Web.Server.Widget
import Path
import Text.Hamlet
import Yesod
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.EmbeddedStatic

data App = App
  { appLogLevel :: !LogLevel,
    appStatic :: !EmbeddedStatic,
    appHTTPManager :: !HTTP.Manager,
    appConnectionPool :: !ConnectionPool,
    appSessionKeyFile :: !(Path Abs File),
    appGoogleAnalyticsTracking :: !(Maybe Text),
    appGoogleSearchConsoleVerification :: !(Maybe Text)
  }

mkYesodData "App" $(makeRelativeToProject "routes.txt" >>= parseRoutesFile)

instance Yesod App where
  defaultLayout widget = do
    app <- getYesod
    messages <- getMessages
    currentRoute <- getCurrentRoute
    navbarPC <- widgetToPageContent $(widgetFile "navbar")
    pageContent <- widgetToPageContent $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")

  makeSessionBackend a = Just <$> defaultClientSessionBackend (60 * 24 * 365 * 10) (fromAbsFile (appSessionKeyFile a))

  shouldLogIO app _ ll = pure $ ll >= appLogLevel app

  maximumContentLengthIO _ _ = pure . Just $ 2 * 1024 * 1024 -- 2 megabytes
  -- Remove any limit for routes which add images or videos.

  authRoute _ = Just $ AuthR LoginR

  -- Split off AccountR, CoachR and AdminR.
  -- List each route explicitly to avoid mistakes.
  isAuthorized _ _ = pure Authorized

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB func = do
    pool <- getsYesod appConnectionPool
    runSqlPool func pool

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = HomeR -- TODO: Set login destination
  logoutDest _ = HomeR
  authenticate Creds {..} = case credsPlugin of
    "impersonation" -> byUser
    "pact" -> byUser -- Our self-defined Auth plugin name
    _ -> pure $ ServerError "Unknown auth plugin"
    where
      byUser = do
        mUser <- case parseUsername credsIdent of
          Left _ -> pure Nothing
          Right un -> liftHandler . runDB . getBy $ UniqueUsername un
        pure $ case mUser of
          Nothing -> UserError $ IdentifierNotFound credsIdent
          Just (Entity userId _) -> Authenticated userId
  onLogin = pure () -- addMessageI "is-success" NowLoggedIn
  authPlugins _ = [pactAuthPlugin]

instance YesodAuthPersist App

genToken :: MonadHandler m => m Html
genToken = do
  alreadyExpired
  req <- getRequest
  let tokenKey = defaultCsrfParamName
  pure $
    case reqToken req of
      Nothing -> mempty
      Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

getFaviconR :: Handler TypedContent
getFaviconR = redirect $ StaticR logo_jpg

navbarRoutesNotLoggedIn :: [(Route App, String)]
navbarRoutesNotLoggedIn =
  [ (HomeR, "Home"),
    (AuthR LoginR, "Log in"),
    (AuthR registerR, "Sign up")
  ]

pactAuthPluginName :: Text
pactAuthPluginName = "pact"

pactLoginHandler :: (Route Auth -> Route App) -> Widget
pactLoginHandler _toParentRoute = do
  messages <- getMessages
  token <- genToken
  setTitle "Login"
  $(widgetFile "auth/login")

pactAuthPlugin :: AuthPlugin App
pactAuthPlugin = AuthPlugin pactAuthPluginName dispatch pactLoginHandler
  where
    dispatch "GET" ["register"] = getRegisterR >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR
    dispatch "POST" ["login"] = postLoginR
    dispatch _ _ = notFound

registerR :: Route Auth
registerR = PluginR pactAuthPluginName ["register"]

loginR :: Route Auth
loginR = PluginR pactAuthPluginName ["login"]

getRegisterR :: AuthHandler App Html
getRegisterR = do
  messages <- getMessages
  token <- genToken
  liftHandler . defaultLayout $ do
    setTitle "Sign up"
    $(widgetFile "auth/register")

usernameField :: Field Handler Username
usernameField = checkMMap toUsername usernameText textField
  where
    toUsername :: Text -> Handler (Either FormMessage Username)
    toUsername t = pure $ mapLeft MsgInvalidEntry $ parseUsername t

registerForm :: FormInput Handler RegisterForm
registerForm =
  RegisterForm
    <$> ireq usernameField "username"
    <*> (mkPassword <$> ireq passwordField "password")
    <*> (mkPassword <$> ireq passwordField "confirmPassword")

postRegisterR :: AuthHandler App TypedContent
postRegisterR = liftHandler $ do
  rf@RegisterForm {..} <- runInputPost registerForm
  mUser <- runDB . getBy $ UniqueUsername registerFormUsername
  case mUser of
    Just _ -> do
      setMessage "Account already exists!"
      redirect $ AuthR registerR
    Nothing -> do
      if not $ confirmPasswords rf
        then do
          addMessageI "is-danger" PassMismatch
          redirect $ AuthR registerR
        else do
          passphraseHash <- liftIO $ hashPassword registerFormPassword
          runDB . insert_ $
            User
              { userName = registerFormUsername,
                userPassword = passphraseHash
              }
          setCredsRedirect
            Creds
              { credsPlugin = pactAuthPluginName,
                credsIdent = usernameText registerFormUsername,
                credsExtra = []
              }

loginForm :: FormInput Handler LoginForm
loginForm =
  LoginForm
    <$> ireq usernameField "username"
    <*> (mkPassword <$> ireq passwordField "password")

postLoginR :: AuthHandler App TypedContent
postLoginR = do
  LoginForm {..} <- liftHandler $ runInputPost loginForm
  mUser <- liftHandler $ runDB $ getBy (UniqueUsername loginFormUsername)
  case mUser of
    Nothing -> loginFail
    Just (Entity _ User {..}) ->
      case checkPassword loginFormPassword userPassword of
        PasswordCheckSuccess ->
          setCredsRedirect
            Creds
              { credsPlugin = pactAuthPluginName,
                credsIdent = usernameText loginFormUsername,
                credsExtra = []
              }
        PasswordCheckFail -> loginFail
  where
    loginFail = loginErrorMessageI LoginR InvalidLogin
