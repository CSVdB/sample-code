{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Data.User where

import Data.Password.Bcrypt
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Servant.API.Generic
import YamlParse.Applicative

data RegisterForm = RegisterForm
  { registerFormUsername :: Username,
    registerFormPassword :: Password,
    registerFormConfirmPassword :: Password
  }
  deriving (Show, Generic)

instance Validity Password where
  validate = trivialValidation

confirmPasswords :: RegisterForm -> Bool
confirmPasswords RegisterForm {..} = unsafeShowPassword registerFormPassword == unsafeShowPassword registerFormConfirmPassword

instance Validity RegisterForm where
  validate rf@RegisterForm {..} =
    mconcat
      [ genericValidate rf,
        declare "ConfirmPassword confirms the password" $ confirmPasswords rf
      ]

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Password
  }
  deriving (Show, Generic)

instance Validity LoginForm

newtype Username = Username
  { usernameText :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check
          (T.length t >= 3)
          "The username is at least three characters long."
      ]

instance PersistField Username where
  toPersistValue (Username t) = PersistText t
  fromPersistValue (PersistText t) = parseUsername t
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance YamlSchema Username where
  yamlSchema = eitherParser parseUsernameOrErr yamlSchema

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

parseUsername :: Text -> Either Text Username
parseUsername = mapLeft T.pack . prettyValidate . Username

parseUsernameOrErr :: Text -> Either String Username
parseUsernameOrErr = prettyValidate . Username

newtype Profile = Profile
  { profileName :: Username
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Profile
