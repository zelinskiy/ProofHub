module Decoders exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required, requiredAt, optional, hardcoded)

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
                            "Admin" -> succeed Admin
                            "Moderator" -> succeed Moderator
                            "Normal" -> succeed Normal
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
           
projectDecoder : Model -> Decoder Project
projectDecoder m =
    field "authors" (list string)
        |> andThen (\authors ->
                        field "categories" (list string)
                   |> andThen (\categories -> 
                                   succeed Project
                              |> requiredAt ["project", "id"] int
                              |> requiredAt ["project", "title"] string
                              |> requiredAt ["project", "added"] string
                              |> requiredAt ["project", "updated"] string
                              |> requiredAt ["project", "proverId"] string
                              |> requiredAt ["project", "longDescription"] string
                              |> requiredAt ["project", "shortDescription"] string
                              |> hardcoded categories
                              |> hardcoded authors
                              |> hardcoded (List.member m.user.email authors)
                              )
                   )
           
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
