{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Schema where

import Database.Persist.TH
import Models.User (User(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserEntity
  Id Int
  username String
  email String
  passwordHash String
  UniqueUsername username
  UniqueEmail email
  deriving Show
|]