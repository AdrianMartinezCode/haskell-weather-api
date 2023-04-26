{-# LANGUAGE OverloadedStrings #-}

module Database.Connection
  ( createUser
  , authenticateUser
  ) where

import Models.User
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

-- Check inside the function defaultConnectInfo to met the fields from the json object
connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
  { connectUser = "your_username"
  , connectPassword = "your_password"
  , connectDatabase = "your_database"
  }

-- TODO change IOError and the responde to another thing
createUser :: UserRegistration -> IO (Either IOError ())
createUser a = return (Right())

-- TODO ídem
authenticateUser :: UserLogin -> IO (Either IOError String)
authenticateUser a = return (Right "")
