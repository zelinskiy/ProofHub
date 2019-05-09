module ProofView exposing (Message(..), update, view)

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
                        , onClick <| SwitchPage ProjectBrowserPage
                        ] []
                , br [] []
                , input [ value "kek.agda"
                        ] []
                , input [ type_ "button"
                        , value "Edit"
                        ] []
                , input [ type_ "button"
                        , value "Remove"
                        ] []
                , br [] []
                , textarea [] [ text "Code" ]
                ]
        rightCol =
            let comment =
                    p [] [ text "user@mail.com wrote: "
                         , input [ type_ "button"
                                 , value "Edit/Save"
                                 ] []
                         , input [ type_ "button"
                                 , value "Delete"
                                 ] []
                         , br [] []
                         , text "Comment"
                         ]
            in div []
                <| List.repeat 3 comment
                    ++ [ textarea [] []
                       , br [] []
                       , input [ type_ "button"
                               , value "Send"
                               ] []
                       ]
    in table [] [
         tr [] [ td [ style "width" "62vw"
                    , style "vertical-align" "top"
                    ] [ leftCol ]
               , td [ style "width" "38vw"
                    , style "vertical-align" "top"
                    ] [ rightCol ]
               ]
        ]
