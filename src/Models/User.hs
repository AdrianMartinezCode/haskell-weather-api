{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.User
  ( User(..)
  , UserRegistration(..)
  , UserLogin(..)
  ) where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data User = User
  { id :: Int
  , username :: String
  , email :: String
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data UserRegistration = UserRegistration
  { regUsername :: String
  , regEmail :: String
  , regPassword :: String
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data UserLogin = UserLogin
  { loginUsername :: String
  , loginPassword :: String
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)
