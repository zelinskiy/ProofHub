module Encoders exposing (..)

import Json.Encode exposing (..)

import Model exposing (..)


encodeUser : User -> Value
encodeUser u =
    object [ ("email", string u.email)
           , ("password", string u.password)
           , ("role", encodeRole u.role)
           , ("avatarPath", string u.avatarPath)
           ]
encodeRole : UserRole -> Value
encodeRole r =
    string <| case r of
                  Normal -> "normal"
                  Moderator -> "moderator"
                  Admin -> "admin"
    
encodeProject : Project -> Value
encodeProject p =
    object [ ("id", int p.id)
           , ("title", string p.title)
           , ("added", string p.added)
           , ("updated", string p.updated)
           , ("proverId", string p.proverId)
           , ("longDescription", string p.longDescription)
           , ("shortDescription", string p.shortDescription)
           ]

encodeDirectory : Directory -> Value
encodeDirectory d =
    object [ ("id", int d.id)
           , ("title", string d.title)
           , ("parentDirectoryId",
                  Maybe.withDefault null
                  <| Maybe.map int d.parentDirectoryId )
           , ("projectId", int d.projectId)
           ]

encodeProof : Proof -> Value
encodeProof p =
    object [ ("id", int p.id)
           , ("title", string p.title)
           , ("text", string p.text)
           , ("longDescription", string p.longDescription)
           , ("shortDescription", string p.shortDescription)
           , ("directoryId", int p.directoryId)
           ]

encodeCategory : Category -> Value
encodeCategory c =
    object [ ("title", string c.title)
           , ("description", string c.description)
           ]

encodeProver : Prover -> Value
encodeProver p =
    object [ ("title", string p.title)
           ]

encodeComment : Comment -> Value
encodeComment c =
    object [ ("id", int c.id)
           , ("text", string c.text)
           , ("proofId", int c.proofId)
           , ("userId", string c.userId)
           ]
                 
                  
