module Dashboard exposing (Message(..), update, view)

import Model exposing (Model)
import Pages exposing (Page(..))
import Settings exposing (..)

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
    let centeredAtrs =
            [ style "margin-left" "auto"
            , style "margin-right" "auto"
            , style "text-align" "center"            
            ]
        leftCol =
            div ([] ++ centeredAtrs)
                [ img [ src model.user.avatarPath
                      , style "width" "100px"
                      ]
                      []
                , br [] []
                , input [ value "email" ] []
                , br [] []
                , input [ value "pass" ] []
                , br [] []
                , input [ type_ "button"
                        , value "Edit/Save"
                        ] []
                , input [ type_ "button"
                        , value "Log out"
                        , onClick <| SwitchPage LoginViewPage
                        ] []
                , hr [] []
                , input [ type_ "button"
                        , value "My projects"
                        ] []
                , input [ type_ "button"
                        , value "All projects"
                        ] []
                , br [] []
                , br [] []
                , select [ value "Prover" ] [ option [] [ text "Prover" ] ]
                , select [ value "Version" ] [ option [] [ text "Version" ] ]
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
                , select [ value "Category3" ] [ option [] [ text "Category3" ] ]
                , br [] []
                , br [] []
                , input [ value "author"
                        , autocomplete True
                        ] []
                ]
        rightCol =
            div ([] ++ centeredAtrs)
                [ input [ value "query" ] []
                , input [ type_ "button"
                        , value "Find"
                        ] []
                , input [ type_ "button"
                        , value "Add"
                        , onClick <| SwitchPage ProjectViewPage
                        ] []
                , hr [] []
                , span [ onClick <| SwitchPage ProjectBrowserPage
                       ] [ text "Project1" ]
                , input [ type_ "button"
                        , value "Edit"
                        , onClick <| SwitchPage ProjectViewPage
                        ] []
                ]
    in table [] [
         tr [] [ td [ style "width" "30vw"
                    , style "vertical-align" "top"
                    ] [ leftCol ]
               , td [ style "width" "70vw"
                    , style "vertical-align" "top"
                    ] [ rightCol ]
               ]
        ]
                   
    
