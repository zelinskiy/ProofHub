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
import Time
import List.Extra exposing (unique)
import Bootstrap.Forms exposing (..)
import Bootstrap.Wells exposing (..)
import Bootstrap.Buttons exposing (..)
import Bootstrap.Grid exposing (..)

type Message 
    = SwitchPage Page
    | LoadUser
    | LoadedUser (Result Http.Error String)
    | BeginEditingUser
    | UpdateUser (User -> String -> User) String
    | SaveUser
    | SavedUser (Result Http.Error String)
      
    | LoadProjects
    | LoadedProjects (Result Http.Error String)
    | LoadProvers
    | LoadMyProjects
    | LoadAllProjects
    | LoadedProvers (Result Http.Error String)
    | LoadCategories
    | LoadedCategories (Result Http.Error String)
    | UpdateQueryText String
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
    | AddSelectedUser
    | RemoveSelectedUser String
    | UpdateSelectedUserEmail String
    | OpenProject Project
    | NextPage
    | PrevPage
      
update : Message -> Model -> (Model, Cmd Message)
update msg m =
    case msg of        
        SwitchPage _ ->
            (m, Cmd.none)
        LoadUser ->
            let cmd = get m "/private/user/me" [] LoadedUser
            in (m, cmd)
        LoadedUser (Ok res) ->
            decodeHandler m userDecoder res (\x u -> { x | user = { u | password = x.user.password }})
        LoadedUser (Err e) ->
            errHandler m e
        BeginEditingUser ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | editingUser = True }
             }, Cmd.none)
        UpdateUser f val ->
            ({ m | user = f m.user val
             }, Cmd.none)
        SaveUser ->
            let route = "/private/user/update"
                cmd = post m route (encodeUser m.user) SavedUser
            in (m, cmd)
        SavedUser (Ok res) ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | editingUser = False }
             }, Cmd.none)
        SavedUser (Err e) ->
            errHandler m e

        LoadMyProjects ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedUserEmails = [ m.user.email ] }
             }, fire LoadProjects)
        LoadAllProjects ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedUserEmails = []
                      , selectedCategoriesTitles = []
                      , selectedProversTitles = []
                      , queryText = ""
                      }
             }, fire LoadProjects)
        UpdateSelectedUserEmail val ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedUser = val }
             }, Cmd.none)
        LoadProjects ->
            let authorsFilter =
                    case m.dashboard.selectedUserEmails of
                        [] -> ""
                        authors ->
                            "&users=" ++ String.join "," authors
                catsFilter =
                    case m.dashboard.selectedCategoriesTitles of
                        [] -> ""
                        cats ->
                            "&categories=" ++ String.join "," cats
                containsFilter =
                    case m.dashboard.queryText of
                        "" -> ""
                        query ->
                            "&contains=" ++ query
                proversFilter =
                    case m.dashboard.selectedProversTitles of
                        [] -> ""
                        provers ->
                            "&provers=" ++ String.join "," provers
                route = "/private/project/search?"
                        ++ authorsFilter
                            ++ catsFilter
                                ++ containsFilter
                                    ++ proversFilter
                cmd = get m route [] LoadedProjects
            in (m, cmd)
        LoadedProjects (Ok res) ->
            decodeHandler m (Decode.list (projectDecoder m)) res (\x ps -> { x | projects = ps })
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
        UpdateQueryText val ->
            ({ m | dashboard = m.dashboard
              |> \d -> { d | queryText = val }
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
        AddSelectedUser ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedUserEmails = unique <| d.selectedUserEmails ++ [d.selectedUser] }
             }, Cmd.none)
        RemoveSelectedUser val ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | selectedUserEmails = List.filter (\x -> x /= val) d.selectedUserEmails }
             }, Cmd.none)
        OpenProject p ->
             ({ m | project = p }, fire <| SwitchPage ProjectBrowserPage)
        NextPage ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | currentPage =
                            if List.length m.projects > d.projectsOnPage * (d.currentPage + 1)
                            then d.currentPage + 1
                            else d.currentPage
                      }
             }, Cmd.none)
        PrevPage ->
            ({ m | dashboard = m.dashboard
             |> \d -> { d | currentPage =
                            if d.currentPage > 0
                            then d.currentPage - 1
                            else d.currentPage
                      }
             }, Cmd.none)

view : Model -> Html Message
view model =
    let proversButtons =
            List.map (\p -> input [ type_ "button"
                                  , value p.title
                                  , onClick <| RemoveProver p
                                  , class "form-control"
                                  ] [])
                model.provers
            -- |> List.intersperse (br [] [])
        categoriesButtons =
            List.map (\c -> input [ type_ "button"
                                  , value c.title
                                  , onClick <| RemoveCategory c
                                  , class "form-control"
                                  ] [])
                model.categories
           -- |> List.intersperse (br [] [])
        selectedCategoriesButtons =
            model.dashboard.selectedCategoriesTitles 
                |> List.map (\c -> input [ type_ "button"
                                         , value c
                                         , onClick <| RemoveSelectedCategory c
                                         , class "form-control"
                                         ] []
                            ) 
        selectedProversButtons =
            model.dashboard.selectedProversTitles
                |> List.map (\x -> input [ type_ "button"
                                         , value x
                                         , onClick <| RemoveSelectedProver x
                                         , class "form-control"
                                         ] []
                            ) 
        selectedUsersButtons =
            List.map (\x -> p [] [ input [ type_ "button"
                                         , value x
                                         , onClick <| RemoveSelectedUser x
                                         , class "form-control"
                                         ] []
                                 ]) model.dashboard.selectedUserEmails
        centeredAtrs =
            [ style "margin-left" "auto"
            , style "margin-right" "auto"
            , style "text-align" "center"            
            ]
        randomString =
            String.fromInt <| Time.posixToMillis model.now
        updAvatar u _ =
            { u | avatarPath =
                  settings.server
                  ++ "/static/avatars/"
                  ++ model.user.email
                  ++ "?r="
                  ++ randomString
            }
        leftCol =
            containerFluid <|
                [ row 
                  [ column [ Medium Twelve ] <| 
                        [ img [ src <| model.user.avatarPath
                              , style "width" "10vw"
                              , class "img-responsive center-block"
                              ]
                              []
                        , br [] []
                        , if model.dashboard.editingUser
                          then Html.form [ enctype "multipart/form-data"
                                         , action <| settings.server ++ "/file?filePath=avatars/" ++ model.user.email
                                         , method "POST"
                                         , target "empty_frame"
                                         ] [ input [ name "file"
                                                   , type_ "file"
                                                   , class "form-control"
                                                   ] []
                                           , input [ type_ "submit"
                                                   , value "Load avatar"
                                                   , onClick <| UpdateUser updAvatar ""
                                                   , class "form-control"
                                                   ] []
                                           ]
                          else span [] []
                        , iframe [ name "empty_frame"
                                 , style "display" "none" ] []
                        , if model.dashboard.editingUser
                          then div []
                              [ br [] []
                              , formInput [ value model.user.email
                                          , onInput <| UpdateUser (\u v -> { u | email = v })
                                          ] []
                              , br [] []
                              ]
                          else well WellSmall [] [ text model.user.email ]
                        , if model.dashboard.editingUser
                          then div []
                              [ formInput [ value model.user.password
                                          , onInput <| UpdateUser (\u v -> { u | password = v })
                                          ] []
                              , br [] []
                              ]
                          else well WellSmall [] [ text model.user.password ]
                        , containerFluid
                            [ row
                              [ column [ Large Six]
                                    [ if  model.dashboard.editingUser
                                      then btn BtnDefault
                                          [ BtnBlock, BtnSmall ]
                                          []
                                          [ onClick SaveUser ]
                                          [ text "Save" ]
                                      else btn BtnDefault
                                          [ BtnBlock, BtnSmall ]
                                          []
                                          [ onClick BeginEditingUser
                                          ] [ text "Edit"]
                                    ]
                              , column [ Large Six]
                                  [ btn BtnDefault
                                        [ BtnBlock, BtnSmall ]
                                        []
                                        [ onClick <| SwitchPage LoginViewPage
                                        ] [ text "Logout"]
                                  ]
                              ]
                            ]
                        , hr [] []
                        , containerFluid
                              [ row
                                [ column [ Large Six]
                                      [ btn BtnDefault
                                            [ BtnBlock, BtnSmall ]
                                            []
                                            [ onClick LoadMyProjects
                                            ] [ text "My"]
                                      ]
                                , column [ Large Six]
                                    [ btn BtnDefault
                                          [ BtnBlock, BtnSmall ]
                                          []
                                          [ onClick LoadAllProjects
                                          ] [ text "All"]
                                    ]
                                ]
                              ]
                        , hr [] []
                        ] ++ selectedProversButtons ++
                        [ br [] []
                        , select [ value "Prover"
                                 , onInput AddSelectedProver
                                 , class "form-control"
                                 ]
                              <| (\xs -> option [] [ text "Prover" ] :: xs)
                              <| List.map (option [] << List.singleton << text << .title)
                              <| List.filter (\c -> not <| List.member c.title model.dashboard.selectedProversTitles)
                              <| model.provers
                        , hr [] []
                        ] ++ selectedCategoriesButtons ++
                        [ br [] []
                        , select [ value "Category"
                                 , onInput AddSelectedCategory
                                 , class "form-control"
                                 ]
                              <| (\xs -> option [] [ text "Category" ] :: xs)
                              <| List.map (option [] << List.singleton << text << .title)
                              <| List.filter (\c -> not <| List.member c.title model.dashboard.selectedCategoriesTitles)
                              <| model.categories
                        , hr [] []
                        ] ++ selectedUsersButtons ++ 
                        [ br [] []
                        , div [ class "input-group" ]                               
                            [ input [ value model.dashboard.selectedUser
                                    , onInput UpdateSelectedUserEmail
                                    , class "form-control"
                                    ] []
                            , div [ class "input-group-btn" ]
                                [ button [ type_ "button"
                                         , onClick AddSelectedUser
                                         , class "form-control"
                                         ] [ text "+" ]
                                ]
                            ]
                        , hr [] []
                        ] ++ proversButtons ++
                        [ br [] []
                        , div [ class "input-group" ]                               
                            [ input [ value model.dashboard.newProver.title
                                    , onInput UpdateNewProverTitle
                                    , class "form-control"
                                    ] []
                            , div [ class "input-group-btn" ]
                                [ button [ type_ "button"
                                         , onClick AddNewProver
                                         , class "form-control"
                                         ] [ text "+" ]
                                ]
                            ]
                        , hr [] []
                        ] ++ categoriesButtons ++ 
                        [ br [] []
                        , div [ class "input-group" ]                               
                            [ input [ value model.dashboard.newCategory.title
                                    , onInput UpdateNewCategoryTitle
                                    , class "form-control"
                                    ] []
                            , div [ class "input-group-btn" ]
                                [ button [ type_ "button"
                                         , onClick AddNewCategory
                                         , class "form-control"
                                         ] [ text "+" ]
                                ]
                            ]
                        , input [ value model.dashboard.newCategory.description
                                , onInput UpdateNewCategoryDescription
                                , class "form-control"
                                ] []
                        ]
                  ]
                ]                        
                        
        projectsList =
            let mapper proj =
                    row
                    [ column [ Medium Two ] []
                    , column [ Medium Seven ]
                          [ input [ type_ "button"
                                  , value proj.title
                                  , onClick <| OpenProject proj
                                  , class "form-control"
                                  ] []
                          ]
                    , column [ Medium Two ]
                          [ input [ type_ "button"
                                  , value "Edit"
                                  , onClick <| EditProject proj
                                  , class "form-control"
                                  ] []
                          , br [] []
                          ]
                    , column [ Medium One ] []
                    ]
            in model.projects
                |> List.drop (model.dashboard.currentPage * model.dashboard.projectsOnPage)
                |> List.take model.dashboard.projectsOnPage
                |> List.map mapper
                |> containerFluid
        rightCol =
            containerFluid <|
                [ row
                  [ column [ Medium Twelve ] <| 
                        [ containerFluid
                          [ row
                            [ column [ Medium Two ] []
                            , column [ Medium Seven ]
                                [ div [ class "input-group" ]                               
                                      [ input [ value model.dashboard.queryText
                                              , onInput UpdateQueryText
                                              , class "form-control"
                                              ] []
                                      , div [ class "input-group-btn" ]
                                          [ button [ type_ "button"
                                                   , onClick LoadProjects
                                                   , class "form-control"
                                                   ] [ text "Go" ]
                                          ]
                                      ]]
                            , column [ Medium Two ]
                                [ input [ type_ "button"
                                        , value "Add"
                                        , onClick AddNewProject
                                        , class "form-control"
                                        ] []
                                ]
                            , column [ Medium One ] []
                            ]
                          ]
                        , hr [] []
                        , projectsList
                        , br [] []
                        , containerFluid
                            [ row
                              [ column [ Medium Four ] []
                              , column [ Medium Two ]
                                  [ input [ type_ "button"
                                          , value "<-"
                                          , onClick PrevPage
                                          , disabled <| model.dashboard.currentPage == 0
                                          , class "form-control"
                                          ] []
                                  ]
                              , column [ Medium Two ]
                                  [ input [ type_ "button"
                                          , value "->"
                                          , onClick NextPage
                                          , disabled <| List.length model.projects
                                              <= model.dashboard.projectsOnPage
                                                  * (model.dashboard.currentPage + 1)
                                          , class "form-control"
                                          ] []
                                  ]
                              , column [ Medium Five ] []
                              ]
                            ]
                        ]
                  ]
                ]
    in container
        [ row
          [ column [ ExtraSmall Six, Small Three, Medium Three, Large Three ]
                [ leftCol ]
          , column [ ExtraSmall Six, Small Nine, Medium Nine, Large Nine ]
                [ rightCol ]
          ]
        ]

    
init : Cmd Message
init =
    Cmd.batch [ fire LoadProjects
              , fire LoadProvers
              , fire LoadCategories
              , fire LoadUser
              ]
