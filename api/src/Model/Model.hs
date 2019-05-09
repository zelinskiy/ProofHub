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
    proverId ProverId
    longDescription String
    shortDescription String
    deriving Eq Show
Directory json
    title String
    parentDirectoryId DirectoryId Maybe 
    projectId ProjectId
    deriving Eq Show
Proof json
    title String
    text String
    longDescription String
    shortDescription String
    directoryId DirectoryId
    deriving Eq Show
ProjectAuthor json
    userId UserId
    projectId ProjectId
    owner Bool
    Primary userId projectId
    deriving Eq Show
ProjectCategory json
    projectId ProjectId
    categoryId CategoryId
    Primary projectId categoryId
    deriving Eq Show
Prover json
    title String
    Primary title
    deriving Eq Show
Category json
    title String
    description String
    Primary title
    deriving Eq Show
Comment json
    text String
    proofId ProofId
    userId UserId
    deriving Eq Show
|]
