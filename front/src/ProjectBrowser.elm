module ProjectBrowser exposing (Message(..), update, view, init)

import Model exposing (Model)
import Pages exposing (Page(..))

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

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
view model =
    div []
        [ input [ type_ "button"
                , value "Return"
                , onClick <| SwitchPage DashboardPage
                ] []
        , input [ type_ "button"
                , value "Add directory"
                ] []
        , input [ type_ "button"
                , value "Add proof"
                ] []
        , br [] []
        , b [] [ text "Directory1" ]
        , input [ type_ "button"
                , value "Edit"
                ] []
        , input [ type_ "button"
                , value "Remove"
                ] []
        , br [] []
        , input [ value "Directory2" ] []
        , input [ type_ "button"
                , value "Edit"
                ] []
        , input [ type_ "button"
                , value "Remove"
                ] []
        , br [] []
        , b [ onClick <| SwitchPage ProofViewPage
            ] [ text "Proof1" ]
        , br [] []
        , b [ onClick <| SwitchPage ProofViewPage
            ] [ text "Proof2" ]
        , br [] []
        ]
          

init : Cmd Message
init =
    Cmd.batch []
