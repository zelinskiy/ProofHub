module ProjectBrowser exposing (Message(..), update, view)

import Model exposing (Model)
import Pages exposing (Page(..))

import Html exposing (..)
import Html.Events exposing (..)

type Message
    = UpdateSomething String
    | SwitchPage Page

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        UpdateSomething val ->
            (model, Cmd.none)
        SwitchPage _ ->
            (model, Cmd.none)

view : Model -> Html Message
view model = p [ onClick <| SwitchPage ProofViewPage ] [ text "ProjectBrowser" ]
