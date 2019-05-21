module ProjectBrowser exposing (Message(..), update, view, init)

import Model exposing (..)
import Pages exposing (Page(..))
import Utils exposing (..)
import Encoders exposing (..)
import Decoders exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import List.Extra
import Maybe.Extra
import Json.Decode as Decode
import Bootstrap.Forms exposing (..)
import Bootstrap.Wells exposing (..)
import Bootstrap.Buttons exposing (..)
import Bootstrap.Grid exposing (..)


type Message
    = SwitchPage Page
    | LoadDirectories
    | LoadedDirectories (Result Http.Error String)
    | LoadProofs
    | LoadedProofs (Result Http.Error String)
    | OpenDirectory (Maybe Directory)
    | OpenProof Proof
    | UpdateDirectoryName Int String
    | ToggleEditDirectory Int
    | RemoveDirectory Int
    | RemovedDirectory Int (Result Http.Error String)
    | AddDirectory
    | AddedDirectory Directory (Result Http.Error String)
    | AddProof 
    | AddedProof Proof (Result Http.Error String)
    | MoveDirectoryUp

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        SwitchPage _ ->
            (model, Cmd.none)
        LoadDirectories ->
            let route = "/private/directory/list/" ++ String.fromInt model.project.id
                cmd = get model route [] LoadedDirectories
            in (model, cmd)
        LoadedDirectories (Ok res) ->
            decodeHandler model (Decode.list directoryDecoder) res (\x ds -> { x | directories = ds })
        LoadedDirectories (Err e) ->
            errHandler model e
        LoadProofs ->
            let did =
                    model.projectBrowser.directory
                        |> Maybe.map .id
                        |> Maybe.withDefault (-1)
                        |> String.fromInt 
                route = "/private/proof/list/" ++ did
                cmd = get model route [] LoadedProofs
            in (model, cmd)
        LoadedProofs (Ok res) ->
            decodeHandler model (Decode.list proofDecoder) res (\x ps -> { x | proofs = ps })
        LoadedProofs (Err e) ->
            errHandler model e
        OpenDirectory d ->
            ( { model | projectBrowser =
                    model.projectBrowser |> \b -> { b | directory = d } }
            , init)
        OpenProof p ->
            ( { model | proofView =
                    model.proofView |> \pv -> { pv | proof = p } }
            , fire <| SwitchPage ProofViewPage)
        RemoveDirectory did ->
            let cmd = delete model
                      ("/private/directory/delete/" ++ String.fromInt did)
                      (RemovedDirectory did)
            in (model, cmd)
        RemovedDirectory did (Ok res) ->
            ({ model | directories = List.filter (\d -> d.id /= did) model.directories }, Cmd.none)
        RemovedDirectory _ (Err e) ->
            errHandler model e
        ToggleEditDirectory did ->
            let cmd =
                    case List.Extra.find (\d -> d.id == did) model.directories of
                        Nothing -> Cmd.none
                        Just d -> post model
                                  ("/private/directory/update/" ++ String.fromInt d.id)
                                  (encodeDirectory d)
                                  LoadedProofs
             in ({ model | directories =
                    List.Extra.updateIf
                    (\d -> d.id == did)
                    (\d -> { d | isEdited = not d.isEdited })
                    model.directories
                 }, cmd)
        UpdateDirectoryName did val ->
            ({ model | directories =
                    List.Extra.updateIf
                    (\d -> d.id == did)
                    (\d -> { d | title = val })
                    model.directories
             } , Cmd.none)
        AddDirectory ->
            let pdId = Maybe.map .id model.projectBrowser.directory
                newDir = { defaultDirectory | parentDirectoryId = pdId
                         , projectId = model.project.id
                         }
                cmd = post model "/private/directory/new"
                      (encodeDirectory newDir)
                          (AddedDirectory newDir)
            in (model, cmd)
        AddedDirectory d (Ok res) ->
            let id = Maybe.withDefault -1 <| String.toInt res
            in ({ model | directories = { d | id = id } :: model.directories }, Cmd.none)
        AddedDirectory _ (Err e) ->
            errHandler model e
        AddProof ->
            case model.projectBrowser.directory of
                Nothing -> 
                    (model, Cmd.none)
                Just pd ->
                    let newProof =
                            { defaultProof | directoryId = pd.id
                            }
                        cmd = post model "/private/proof/new"
                              (encodeProof newProof)
                              (AddedProof newProof)
                    in (model, cmd)
        AddedProof p (Ok res) ->
            let id = Maybe.withDefault -1 <| String.toInt res
            in ({ model | proofs = { p | id = id
                                   , title = "Proof " ++ String.fromInt id
                                   } :: model.proofs
                }, Cmd.none)
        AddedProof _ (Err e) ->
            errHandler model e
        MoveDirectoryUp ->
            let parentDir =
                    model.projectBrowser.directory
                          |> Maybe.map .parentDirectoryId
                          |> Maybe.withDefault (Just -1)
                          |> Maybe.map (\did -> List.Extra.find (\d -> d.id == did) model.directories)
                          |> Maybe.Extra.join
            in case model.projectBrowser.directory of                   
                   Just d ->
                       (model, fire <| OpenDirectory parentDir)                       
                   Nothing -> 
                       (model, fire <| SwitchPage DashboardPage)
            

                
view : Model -> Html Message
view model =
    let editable = model.project.editable
        proofs =
            model.proofs
                |> List.filter (\p ->
                                    Just p.directoryId
                                    == Maybe.map .id model.projectBrowser.directory)
                |> List.map (\pf ->                                 
                                 row
                                 [ column [ Medium Eight ]
                                       [ input [ type_ "button"
                                               , onClick <| OpenProof pf
                                               , class "form-control"
                                               , value pf.title
                                               ] []
                                       , br [] []
                                       ]
                                 ])
                |> containerFluid 
                    
        directories =
            model.directories
                |> List.filter (\d ->
                                    case model.projectBrowser.directory of
                                        Nothing ->
                                            d.parentDirectoryId == Nothing
                                        Just pd ->
                                            d.parentDirectoryId == Just pd.id)
                |> List.map (\d ->
                                 row
                                 [ column [ Medium Eight ]
                                       [ if d.isEdited
                                         then input [ value d.title
                                                    , class "form-control"
                                                    , onInput <| UpdateDirectoryName d.id
                                                    , disabled <| not editable
                                                    ] []
                                         else well WellSmall
                                             [ onClick <| OpenDirectory (Just d) ]
                                             [ text <| d.title ]
                                       ]
                                 , column [ Medium Two ]
                                     [ input [ type_ "button"
                                              , value <| if d.isEdited then "Save" else "Edit"
                                              , onClick <| ToggleEditDirectory d.id
                                             , class "form-control"
                                              , disabled <| not editable
                                              ] []
                                     ]
                                 , column [ Medium Two ]
                                     [ input [ type_ "button"
                                             , value "Remove"
                                             , onClick <| RemoveDirectory d.id
                                             , class "form-control"
                                             , disabled <| not editable
                                             ] []
                                     , br [] []
                                     ]
                                 ])
                |> containerFluid 
    in container
        [ row
          [ column [ Medium Two ] []
          , column [ Medium Ten ]
              [ containerFluid
                [ row
                  [ column [ Medium Two ]
                        [ input [ type_ "button"
                                , value "Return"
                                , class "form-control"
                                , onClick MoveDirectoryUp
                                ] []
                        ]
                  , column [ Medium Three ]
                      [ input [ type_ "button"
                              , value "Add directory"
                              , onClick AddDirectory
                              , class "form-control"
                              , disabled <| not editable
                              ] []
                      ]
                  , column [ Medium Three ]
                      [ input [ type_ "button"
                              , value "Add proof"
                              , onClick AddProof
                              , class "form-control"
                              , disabled
                                    <| Maybe.Extra.isNothing model.projectBrowser.directory || not editable
                              ] []
                      ]
                  ]
                ]
              , br [] []
              , directories
              , hr [] []
              , proofs 
              ]
          , column [ Medium Two ] []
          ]
        ]
        

init : Cmd Message
init =
    Cmd.batch [ fire LoadDirectories
              , fire LoadProofs
              ]
