module ProjectView exposing (Message(..), update, view, init)

import Model exposing (Model)
import Pages exposing (..)
import Utils exposing (..)
import Encoders exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode

type Message
    = SwitchPage Page
    | UpdateTitle String
    | UpdateProver String
    | AddCategory String
    | RemoveCategory String
    | UpdateNewAuthor String
    | AddAuthor
    | RemoveAuthor String
    | UpdateShortDescription String
    | UpdateLongDescription String
    | Save
    | Saved (Result Http.Error String)
    | Remove
    | Removed (Result Http.Error String)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        SwitchPage _ ->
            (model, Cmd.none)
        UpdateTitle val ->
            (updProject model <| \p -> { p | title = val }, Cmd.none)
        UpdateProver val ->
            (updProject model <| \p -> { p | proverId = val }, Cmd.none)
        AddCategory val ->
            (updProject model <| \p -> { p | categoriesTitles = p.categoriesTitles ++ [ val ] }, Cmd.none)
        UpdateNewAuthor val ->
            (updProjectView model <| \m -> { m | newAuthor = val }
            , Cmd.none)
        AddAuthor ->
            (updProject model <| \p -> { p | authorsEmails = p.authorsEmails ++ [ model.projectView.newAuthor ] }
            , Cmd.none)
        RemoveCategory val ->
            (updProject model <| \p -> { p | categoriesTitles = List.filter (\x -> x /= val) p.categoriesTitles }, Cmd.none)
        RemoveAuthor val ->
            (updProject model <| \p -> { p | authorsEmails = List.filter (\x -> x /= val) p.authorsEmails }
            , Cmd.none)
        UpdateShortDescription val ->
            (updProject model <| \p -> { p | shortDescription = val }, Cmd.none)
        UpdateLongDescription val ->
            (updProject model <| \p -> { p | longDescription = val }, Cmd.none)
        Save ->
            let route = if model.project.id == -1
                        then "/private/project/new"
                        else "/private/project/update/" ++ String.fromInt model.project.id
                model_ =
                    updProject model
                        <| \p -> { p | added = timeNow model
                                 , updated = timeNow model
                                 }
                cmd = post model_ route (encodeProject model.project) Saved
            in (model, cmd)
        Saved (Ok res) ->
            ( { model | debug = res }, Cmd.none)
        Saved (Err e) ->
            errHandler model e
        Remove ->            
            let route = "/private/project/delete/" ++ String.fromInt model.project.id
                cmd = if model.project.id == -1
                      then fire <| SwitchPage DashboardPage
                      else Cmd.batch [ delete model route Removed
                                     , fire <| SwitchPage DashboardPage
                                     ]
            in (model, cmd)
        Removed (Ok res) ->
            ( { model | debug = res }, Cmd.none)
        Removed (Err e) ->
            errHandler model e

view : Model -> Html Message
view model =
    let leftCol =
            div []
                [ input [ type_ "button"
                        , value "Return"
                        , onClick <| SwitchPage DashboardPage
                        ] []
                , br [] []
                , input [ value model.project.title
                        , onInput UpdateTitle
                        ] []
                , input [ type_ "button"
                        , value <| if model.project.id == -1
                                   then "Add"
                                   else "Update"
                        , onClick Save
                        ] []
                , input [ type_ "button"
                        , value "Remove"
                        , onClick Remove
                        ] []
                , br [] []
                , input [ value model.project.shortDescription
                        , onInput UpdateShortDescription
                        ] []
                , br [] []
                , textarea [ onInput UpdateLongDescription ]
                    [ text model.project.longDescription ]
                , br [] []
                ]
        categories =
            model.project.categoriesTitles
                |> List.map (\x -> input [ type_ "button"
                                         , onClick (RemoveCategory x)
                                         , value x
                                         ] [])
                |> List.intersperse (br [] [])
        authors =
            model.project.authorsEmails
                |> List.map (\x -> input [ type_ "button"
                                         , onClick (RemoveAuthor x)
                                         , value x
                                         ] [])
                |> List.intersperse (br [] [])
        rightCol =
            div [] <| 
                [ select
                      [ value model.project.proverId
                      , onInput UpdateProver
                      ] <| List.map (option [] << List.singleton << text << .title)
                      <| model.provers
                , br [] []
                , br [] []
                ] ++ categories ++
                [ br [] []
                , select
                      [ value "Add category"
                      , onInput AddCategory
                      ] <| (\opts -> option [] [ text "Add category" ] :: opts)
                      <| List.map (option [] << List.singleton << text << .title)
                      <| List.filter (\c -> not <| List.member c.title model.project.categoriesTitles)
                      <| model.categories
                , br [] []
                , br [] []
                ] ++ authors ++ 
                [ br [] []
                , input [ value model.projectView.newAuthor
                        , onInput UpdateNewAuthor
                        , autocomplete True
                        ] []
                , input [ type_ "button"
                        , value "+"
                        , onClick AddAuthor
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

        
init : Cmd Message
init =
    Cmd.batch []
