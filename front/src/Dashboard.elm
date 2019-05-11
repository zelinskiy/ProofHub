module Dashboard exposing (Message(..), update, view, init)

import Model exposing (..)
import Pages exposing (Page(..))
import Settings exposing (..)
import Utils exposing (..)
import Encoders exposing (..)
import Decoders exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode

type Message 
    = UpdateSomething String
    | SwitchPage Page
    | LoadProjects
    | LoadedProjects (Result Http.Error String)
    | LoadProvers
    | LoadedProvers (Result Http.Error String)
    | LoadCategories
    | LoadedCategories (Result Http.Error String)
    | EditProject Project
    | UpdateNewCategoryTitle String
    | UpdateNewCategoryDescription String    
    | UpdateNewProverTitle String
    | AddNewCategory
    | AddedNewCategory (Result Http.Error String)
    | AddNewProver
    | AddedNewProver (Result Http.Error String)
    | RemoveCategory Category
    | RemovedCategory Category (Result Http.Error String)
    | RemoveProver Prover
    | RemovedProver Prover (Result Http.Error String)
    | AddNewProject
    | AddSelectedCategory String
    | RemoveSelectedCategory String
    | AddSelectedProver String
    | RemoveSelectedProver String
    | OpenProject Project
      
update : Message -> Model -> (Model, Cmd Message)
update msg m =
    case msg of
        UpdateSomething val ->
            (m, Cmd.none)
        SwitchPage _ ->
            (m, Cmd.none)
        LoadProjects ->
            let cmd = get m "/private/project/search" [] LoadedProjects
            in (m, cmd)
        LoadedProjects (Ok res) ->
            decodeHandler m (Decode.list projectDecoder) res (\x ps -> { x | projects = ps })
        LoadedProjects (Err e) ->
            errHandler m e
        LoadProvers ->
            let cmd = get m "/private/prover/list" [] LoadedProvers
            in (m, cmd)
        LoadedProvers (Ok res) ->
            decodeHandler m (Decode.list proverDecoder) res (\x ps -> { x | provers = ps })
        LoadedProvers (Err e) ->
            errHandler m e
        LoadCategories ->
            let cmd = get m "/private/category/list" [] LoadedCategories
            in (m, cmd)
        LoadedCategories (Ok res) ->
            decodeHandler m (Decode.list categoryDecoder) res (\x cs -> { x | categories = cs })
        LoadedCategories (Err e) ->
            errHandler m e                
        EditProject p ->
            let m_ = { m | project = p }
            in (m_, fire <| SwitchPage ProjectViewPage )
        UpdateNewCategoryTitle val ->
            ( { m | dashboard = m.dashboard
              |> \d -> { d | newCategory = d.newCategory
                       |> \c -> { c | title = val } }
              }
            , Cmd.none)
        UpdateNewCategoryDescription val ->
            ({ m | dashboard = m.dashboard
              |> \d -> { d | newCategory = d.newCategory
                       |> \c -> { c | description = val } }
              }, Cmd.none)
        UpdateNewProverTitle val ->
            ({ m | dashboard = m.dashboard
              |> \d -> { d | newProver = d.newProver
                       |> \c -> { c | title = val } }
              }, Cmd.none)
        AddNewProver ->
            let cmd = post m "/private/prover/new"
                      (encodeProver m.dashboard.newProver)
                          AddedNewProver
            in (m, cmd)
        AddNewCategory ->
            let cmd = post m "/private/category/new"
                      (encodeCategory m.dashboard.newCategory)
                          AddedNewCategory
            in (m, cmd)
        AddedNewCategory (Ok _) ->
            ({ m | categories = m.dashboard.newCategory :: m.categories }, Cmd.none)
        AddedNewProver (Ok _)->
            ({ m | provers = m.dashboard.newProver :: m.provers }, Cmd.none)
        AddedNewCategory (Err e) ->
            errHandler m e
        AddedNewProver (Err e)->
            errHandler m e
        RemoveProver p ->
            let cmd = delete m ("/private/prover/delete/" ++ p.title)
                      (RemovedProver p)
            in (m, cmd)
        RemoveCategory c ->
            let cmd = delete m ("/private/category/delete/" ++ c.title)
                      (RemovedCategory c)
            in (m, cmd)
        RemovedCategory c (Ok _) ->
            ({ m | categories = List.filter (\x -> x.title /= c.title) m.categories }, Cmd.none)
        RemovedProver p (Ok _)->
            ({ m | provers = List.filter (\x -> x.title /= p.title) m.provers }, Cmd.none)
        RemovedCategory _ (Err e) ->
            errHandler m e
        RemovedProver _ (Err e)->
            errHandler m e
        AddNewProject ->
            ({ m | project = defaultProject }, fire <| SwitchPage ProjectViewPage)
        AddSelectedCategory val ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedCategoriesTitles = d.selectedCategoriesTitles ++ [val] }
             }, Cmd.none)
        RemoveSelectedCategory val ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedCategoriesTitles = List.filter (\x -> x /= val) d.selectedCategoriesTitles }
             }, Cmd.none)
        AddSelectedProver val ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedProversTitles = d.selectedProversTitles ++ [val] }
             }, Cmd.none)
        RemoveSelectedProver val ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedProversTitles = List.filter (\x -> x /= val) d.selectedProversTitles }
             }, Cmd.none)
        OpenProject p ->
             ({ m | project = p}, fire <| SwitchPage ProjectBrowserPage)

view : Model -> Html Message
view model =
    let proversButtons =
            List.map (\p -> input [ type_ "button"
                                  , value p.title
                                  , onClick <| RemoveProver p
                                  ] [])
                model.provers
            |> List.intersperse (br [] [])
        categoriesButtons =
            List.map (\c -> input [ type_ "button"
                                  , value c.title
                                  , onClick <| RemoveCategory c
                                  ] [])
                model.categories
           |> List.intersperse (br [] [])
        selectedCategoriesButtons =
            List.map (\c -> p [] [ input [ type_ "button"
                                         , value c
                                         , onClick <| RemoveSelectedCategory c
                                         ] []
                                 ]) model.dashboard.selectedCategoriesTitles
        selectedProversButtons =
            List.map (\x -> p [] [ input [ type_ "button"
                                         , value x
                                         , onClick <| RemoveSelectedProver x
                                         ] []
                                 ]) model.dashboard.selectedProversTitles
        centeredAtrs =
            [ style "margin-left" "auto"
            , style "margin-right" "auto"
            , style "text-align" "center"            
            ]
        leftCol =
            div ([] ++ centeredAtrs) <|
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
                ] ++ selectedProversButtons ++
                [ select [ value "Prover"
                         , onInput AddSelectedProver
                         ]
                      <| (\xs -> option [] [ text "Prover" ] :: xs)
                      <| List.map (option [] << List.singleton << text << .title)
                      <| List.filter (\c -> not <| List.member c.title model.dashboard.selectedProversTitles)
                      <| model.provers
                , br [] []
                , br [] []
                ] ++ selectedCategoriesButtons ++
                [ select [ value "Category"
                         , onInput AddSelectedCategory
                         ]
                      <| (\xs -> option [] [ text "Category" ] :: xs)
                      <| List.map (option [] << List.singleton << text << .title)
                      <| List.filter (\c -> not <| List.member c.title model.dashboard.selectedCategoriesTitles)
                      <| model.categories
                , br [] []
                , br [] []
                , input [ value "author"
                        , autocomplete True
                        ] []
                , br [] []
                , hr [] []
                , br [] []
                ] ++ proversButtons ++
                [ br [] []
                , input [ value model.dashboard.newProver.title
                        , onInput UpdateNewProverTitle
                        ] []
                , input [ type_ "button"
                        , value "+"
                        , onClick AddNewProver
                        ] []
                , br [] []
                , br [] []
                ] ++ categoriesButtons ++ 
                [ br [] []
                , input [ value model.dashboard.newCategory.title
                        , onInput UpdateNewCategoryTitle
                        ] []
                , input [ value model.dashboard.newCategory.description
                        , onInput UpdateNewCategoryDescription
                        ] []
                , input [ type_ "button"
                        , value "+"
                        , onClick AddNewCategory
                        ] []
                ]
        projectsList =
            let mapper proj =
                    p [] [ span [ onClick <| OpenProject proj
                                ] [ text proj.title ]
                         , input [ type_ "button"
                                 , value "Edit"
                                 , onClick <| EditProject proj
                                 ] []
                         ]
            in model.projects
                |> List.map mapper                                 
        rightCol =
            div ([] ++ centeredAtrs) <| 
                [ input [ value "query" ] []
                , input [ type_ "button"
                        , value "Find"
                        ] []
                , input [ type_ "button"
                        , value "Add"
                        , onClick AddNewProject
                        ] []
                , hr [] []
                ] ++ projectsList ++ 
                [       
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

    
init : Cmd Message
init =
    Cmd.batch [ fire LoadProjects
              , fire LoadProvers
              , fire LoadCategories
              ]
