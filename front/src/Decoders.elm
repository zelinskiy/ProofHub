module Decoders exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)

import Model exposing (..)


userDecoder : Decoder User
userDecoder =
    succeed User
        |> required "email" string
        |> required "password" string
        |> required "role" roleDecoder
        |> required "avatarPath" string

roleDecoder : Decoder UserRole
roleDecoder =
    string
        |> andThen (\x ->
                        case x of
                            "admin" -> succeed Admin
                            "moderator" -> succeed Moderator
                            "normal" -> succeed Normal
                            r -> fail <| "Unknown role " ++ r
                   )
           
categoryDecoder : Decoder Category
categoryDecoder =
    succeed Category
        |> required "title" string
        |> required "description" string

proverDecoder : Decoder Prover
proverDecoder =
    succeed Prover
        |> required "title" string

projectDecoder : Decoder Project
projectDecoder =
    succeed Project
        |> required "id" int
        |> required "title" string
        |> required "added" string
        |> required "updated" string
        |> required "proverId" string
        |> required "longDescription" string
        |> required "shortDescription" string
        |> hardcoded []
        |> hardcoded []

directoryDecoder : Decoder Directory
directoryDecoder =
    succeed Directory
        |> required "id" int
        |> required "title" string
        |> required "parentDirectoryId" (nullable int)
        |> required "projectId" int
        |> hardcoded False

proofDecoder : Decoder Proof
proofDecoder =
    succeed Proof
        |> required "id" int
        |> required "title" string
        |> required "text" string
        |> required "longDescription" string
        |> required "shortDescription" string
        |> required "directoryId" int

commentDecoder : Decoder Comment
commentDecoder =
    succeed Comment
        |> required "id" int
        |> required "text" string
        |> required "proofId" int
        |> required "userId" string
        |> hardcoded False
