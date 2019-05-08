{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model.Model where

import Database.Persist.TH
import Data.Time.Clock

import Model.UserRole as UserRole

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"]
  [persistLowerCase|
User sql=users json
    email String
    password String
    role UserRole
    avatarPath String
    Primary email
    deriving Eq Show
Project json
    title String
    added UTCTime
    updated UTCTime
    prover Prover
    longDescription String
    shortDescription String
    deriving Eq Show
Directory json
    title String
    parentDirectory Directory Maybe 
    project Project
    deriving Eq Show
Proof json
    title String
    text String
    longDescription String
    shortDescription String
    directory Directory
    deriving Eq Show
ProofAuthor json
    user UserId
    project ProjectId
    owner Bool
    Primary user project
    deriving Eq Show
Prover json
    title String
    version String
    Primary title version
    deriving Eq Show
Category json
    title String
    description String
    Primary title
    deriving Eq Show
Comment json
    text String
    proof Proof
    deriving Eq Show
|]
