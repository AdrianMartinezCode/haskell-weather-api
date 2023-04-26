{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.User
  ( UserAPI
  , userServer
  ) where

import Servant
import Models.User
import Database.Connection
import Control.Monad.IO.Class (liftIO)

type UserAPI =
       "register" :> ReqBody '[JSON] UserRegistration :> Post '[JSON] ()
  :<|> "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] String

userServer :: Server UserAPI
userServer = registerUser
        :<|> loginUser

registerUser :: UserRegistration -> Handler ()
registerUser registration = do
  result <- liftIO $ createUser registration
  case result of
    Left err -> throwError err400 { errBody = "Error registering user" }
    Right () -> return ()

loginUser :: UserLogin -> Handler String
loginUser loginData = do
  result <- liftIO $ authenticateUser loginData
  case result of
    Left err -> throwError err401 { errBody = "Invalid username or password" }
    Right token -> return token