module ProjectView exposing (Message(..), update, view)

import Model exposing (Model)
import Pages exposing (..)

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
    let leftCol =
            div []
                [ input [ type_ "button"
                        , value "Return"
                        , onClick <| SwitchPage DashboardPage
                        ] []
                , br [] []
                , input [ value "name" ] []
                , input [ type_ "button"
                        , value "Add/Update"
                        ] []
                , input [ type_ "button"
                        , value "Remove"
                        ] []
                , br [] []
                , input [ value "short description" ] []
                , br [] []
                , textarea [] [ text "long description" ]
                , br [] []
                ]
        rightCol =
            div []
                [ select
                      [ value "Prover" ]
                      [ option [] [ text "Prover" ] ]
                , select
                      [ value "Version" ]
                      [ option [] [ text "Version" ] ]
                , br [] []
                , br [] []
                , input [ type_ "button"
                        , value "Category1"
                        ] []
                , br [] []
                , input [ type_ "button"
                        , value "Category2"
                        ] []
                , br [] []
                , select
                      [ value "Category3" ]
                      [ option [] [ text "Category3" ] ]
                , br [] []
                , br [] []
                , input [ type_ "button"
                        , value "Author1"
                        ] []
                , br [] []
                , input [ value "author"
                        , autocomplete True
                        ] []
                ]
    in table [] [
         tr [] [ td [ style "width" "70vw"
                    , style "vertical-align" "top"
                    ] [ leftCol ]
               , td [ style "width" "30vw"
                    , style "vertical-align" "top"
                    ] [ rightCol ]
               ]
        ]
