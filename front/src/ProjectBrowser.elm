module ProjectBrowser exposing (Message(..), update, view, init)

import Model exposing (..)
import Pages exposing (Page(..))
import Utils exposing (..)
import Encoders exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import List.Extra
import Maybe.Extra

type Message
    = SwitchPage Page
    | LoadDirectories
    | LoadedDirectories (Result Http.Error String)
    | LoadProofs
    | LoadedProofs (Result Http.Error String)
    | OpenDirectory Directory
    | OpenProof Proof
    | UpdateDirectoryName Int String
    | ToggleEditDirectory Int
    | RemoveDirectory Int
    | RemovedDirectory Int (Result Http.Error String)
    | AddDirectory
    | AddedDirectory Directory (Result Http.Error String)
    | AddProof 
    | AddedProof Proof (Result Http.Error String)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        SwitchPage _ ->
            (model, Cmd.none)
        LoadDirectories ->
            let route = "/private/directories/list/" ++ String.fromInt model.project.id
                cmd = get model route [] LoadedDirectories
            in (model, cmd)
        LoadedDirectories (Ok res) ->
            decodeHandler m (Decode.list directoryDecoder) res (\x ps -> { x | directories = ds })
        LoadedDirectories (Err e) ->
            errHandler model e
        LoadProofs ->
            (model, Cmd.none)
        LoadedProofs (Ok res) ->
            (model, Cmd.none)
        LoadedProofs (Err e) ->
            errHandler model e
        OpenDirectory d ->
            ( { model | projectBrowser =
                    model.projectBrowser |> \b -> { b | directory = Just d } }
            , Cmd.none)
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
            in ({ model | proofs = { p | id = id } :: model.proofs }, Cmd.none)
        AddedProof _ (Err e) ->
            errHandler model e
            

                
view : Model -> Html Message
view model =
    let proofs =
            model.proofs
                |> List.map (\pf ->
                                 p [] [ text pf.title ])
        directories =
            model.directories
                |> List.filter (\d ->
                                    case model.projectBrowser.directory of
                                        Nothing ->
                                            d.parentDirectoryId == Nothing
                                        Just pd ->
                                            d.parentDirectoryId == Just pd.id)
                |> List.map (\d ->
                                 p [] [ if d.isEdited
                                        then input [ value d.title
                                                   , onInput <| UpdateDirectoryName d.id
                                                   ] []
                                        else text <| d.title ++ " " ++ String.fromInt d.id
                                      , input [ type_ "button"
                                              , value <| if d.isEdited then "Save" else "Edit"
                                              , onClick <| ToggleEditDirectory d.id
                                              ] []
                                      , input [ type_ "button"
                                              , value "Remove"
                                              , onClick <| RemoveDirectory d.id
                                              ] []
                                      ])
    in div [] <| 
        [ input [ type_ "button"
                , value "Return"
                , onClick <| SwitchPage DashboardPage
                ] []
        , input [ type_ "button"
                , value "Add directory"
                , onClick AddDirectory
                ] []
        , input [ type_ "button"
                , value "Add proof"
                , onClick AddProof
                , disabled <| Maybe.Extra.isJust model.projectBrowser.directory
                ] []
        , hr [] []
        ] ++ directories ++ 
        [ hr [] []
        ] ++ proofs
          

init : Cmd Message
init =
    Cmd.batch [ fire LoadDirectories
              , fire LoadProofs
              ]
