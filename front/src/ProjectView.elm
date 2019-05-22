module ProjectView exposing (Message(..), update, view, init)

import Model exposing (..)
import Pages exposing (..)
import Utils exposing (..)
import Encoders exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Bootstrap.Forms exposing (..)
import Bootstrap.Wells exposing (..)
import Bootstrap.Buttons exposing (..)
import Bootstrap.Grid exposing (..)

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
            (updProject model <| \p ->
                 { p | title = val }
            , Cmd.none)
        UpdateProver val ->
            (updProject model <| \p ->
                 { p | proverId = if val == "Pick a prover"
                                  then p.proverId
                                  else val }
            , Cmd.none)
        AddCategory val ->
            (updProject model <| \p ->
                 { p | categoriesTitles = p.categoriesTitles ++ [ val ] }, Cmd.none)
        UpdateNewAuthor val ->
            (updProjectView model <| \m ->
                 { m | newAuthor = val }
            , Cmd.none)
        AddAuthor ->
            (updProject model <| \p ->
                 { p | authorsEmails = p.authorsEmails ++ [ model.projectView.newAuthor ] }
            , Cmd.none)
        RemoveCategory val ->
            (updProject model <| \p ->
                 { p | categoriesTitles = List.filter (\x -> x /= val) p.categoriesTitles }
            , Cmd.none)
        RemoveAuthor val ->
            (updProject model <| \p ->
                 { p | authorsEmails = List.filter (\x -> x /= val) p.authorsEmails }
            , Cmd.none)
        UpdateShortDescription val ->
            (updProject model <| \p ->
                 { p | shortDescription = val }
            , Cmd.none)
        UpdateLongDescription val ->
            (updProject model <| \p ->
                 { p | longDescription = val }
            , Cmd.none)
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
        Saved (Ok pid) ->
            let pid_ = Maybe.withDefault (-1) <| String.toInt pid
                cmd = fire <| SwitchPage ProjectBrowserPage
                model_ =
                    updProject model
                        <| \p -> { p | id = pid_ }
            in (model_, cmd)
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
            ( { model | project = defaultProject
              , projects = List.filter (\p -> p.id /= model.project.id) model.projects
              }, Cmd.none)
        Removed (Err e) ->
            errHandler model e

view : Model -> Html Message
view model =
    let editable = model.project.editable
        leftCol =
            containerFluid
                [ row
                      [ column
                            [ Medium Three ]
                            [ input [ type_ "button"
                                    , value "Return"
                                    , onClick <| SwitchPage DashboardPage
                                    , class "form-control"
                                    ] []
                            ]
                      , column
                            [ Medium Three ]
                            []
                      , column
                            [ Medium Three ]
                            [ input [ type_ "button"
                                    , value <| if model.project.id == -1
                                               then "Add"
                                               else "Update"
                                    , onClick Save
                                    , disabled <| not editable
                                    , class "form-control btn-info"
                                    ] []
                            ]
                      , column
                            [ Medium Three ]
                            [ input [ type_ "button"
                                    , value "Delete"
                                    , onClick Remove
                                    , disabled <| not editable
                                    , class "form-control btn-danger"
                                    ] []
                            ]
                      ]
                , hr [] []
                , row [ column
                            [ Medium Twelve ]
                            [ input [ value model.project.title
                                    , onInput UpdateTitle
                                    , disabled <| not editable
                                    , class "form-control"
                                    ] []
                            ]
                      ]
                , br [] []
                , row [ column
                            [ Medium Twelve ]
                            [ input [ value model.project.shortDescription
                                    , onInput UpdateShortDescription
                                    , disabled <| not editable
                                    , class "form-control"
                                    ] []
                            ]
                      ]
                , br [] []
                , row [ column
                            [ Medium Twelve ]
                            [ textarea [ onInput UpdateLongDescription
                                       , disabled <| not editable
                                       , class "form-control"
                                       ]
                                  [ text model.project.longDescription ]
                            ]
                      ]
                ]
        categories =
            model.project.categoriesTitles
                |> List.map (\x ->
                                 row [ column
                                           [ Medium Twelve ]
                                           [ input [ type_ "button"
                                                   , onClick (RemoveCategory x)
                                                   , disabled <| not editable
                                                   , value x
                                                   , class "form-control"
                                                   ] []
                                           , br [] []
                                           ]
                                     ])
        authors =
            model.project.authorsEmails
                |> List.map (\x ->
                                 row [ column
                                           [ Medium Twelve ]
                                           [ input [ type_ "button"
                                                   , onClick (RemoveAuthor x)
                                                   , disabled <| not editable
                                                   , value x
                                                   , class "form-control"
                                                   ] []
                                           , br [] []
                                           ]
                                     ])
        rightCol =
            containerFluid <| 
                [ row [ column
                            [ Medium Twelve ]
                            [ select
                                  [ value
                                        <| if List.member model.project.proverId
                                            (List.map .title model.provers)
                                           then model.project.proverId 
                                           else "Pick a prover"
                                  , onInput UpdateProver
                                  , disabled <| not editable
                                  , class "form-control"
                                  ] <| (\opts -> option [] [ text "Pick a prover" ] :: opts)
                                  <| List.map (option [] << List.singleton << text << .title)
                                  <| model.provers
                            ]
                      ]
                , hr [] []
                ] ++ categories ++
                [ row [ column
                            [ Medium Twelve ]
                            [ select
                                  [ value "Add category"
                                  , onInput AddCategory
                                  , disabled <| not editable
                                  , class "form-control"
                                  ] <| (\opts -> option [] [ text "Add category" ] :: opts)
                                  <| List.map (option [] << List.singleton << text << .title)
                                  <| List.filter (\c -> not
                                                      <| List.member c.title model.project.categoriesTitles)
                                  <| model.categories
                            ]
                      ]
                , hr [] []
                ] ++ authors ++ 
                [ div [ class "input-group" ]                               
                    [ input [ value model.projectView.newAuthor
                            , onInput UpdateNewAuthor
                            , autocomplete True
                            , class "form-control"
                            ] []
                    , div [ class "input-group-btn" ]
                        [ input [ type_ "button"
                                , value "+"
                                , onClick AddAuthor
                                , class "form-control"
                                ] []
                        ]
                    ]
                ]
    in container
        [ row
          [ column [ ExtraSmall One, Small Two, Medium Two, Large Two ]
                []
          , column [ ExtraSmall Five, Small Five, Medium Five, Large Five ]
                [ leftCol ]
          , column [ ExtraSmall Five, Small Three, Medium Three, Large Three ]
                [ rightCol ]
          , column [ ExtraSmall One, Small Two, Medium Two, Large Two ]
                []
          ]
        ]

        
init : Cmd Message
init =
    Cmd.batch []
