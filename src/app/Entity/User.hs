{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.User where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.Persist.TH
  ( MkPersistSettings (mpsGenerateLenses),
    mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings {mpsGenerateLenses = True}, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    name String
    age Int
    email String
    registrationDate Day
    deriving Show Generic
Country
   code Text
   name Text
   deriving Show Generic
|]
