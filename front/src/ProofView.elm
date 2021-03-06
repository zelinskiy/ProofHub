module ProofView exposing (Message(..), update, view, init)

import Model exposing (..)
import Pages exposing (..)
import Utils exposing (..)
import Decoders exposing (..)
import Encoders exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import List.Extra
import Bootstrap.Forms exposing (..)
import Bootstrap.Wells exposing (..)
import Bootstrap.Buttons exposing (..)
import Bootstrap.Grid exposing (..)

-- TODO:
-- Separate Proofs from Comments

type Message
    = SwitchPage Page
    {- Proofs -}
    | UpdateProof (Proof -> String -> Proof) String
    | ToggleEditMode
    | RemoveProof
    | RemovedProof (Result Http.Error String)
    | SaveProof
    | SavedProof (Result Http.Error String)
    {- Comments -}
    | LoadComments
    | LoadedComments (Result Http.Error String)
    | UpdateNewComment (Comment -> String -> Comment) String
    | AddComment
    | AddedComment (Result Http.Error String)
    | UpdateComment Int (Comment -> String -> Comment) String
    | SaveComment Int
    | SavedComment Int (Result Http.Error String)
    | RemoveComment Int
    | RemovedComment Int (Result Http.Error String)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        SwitchPage _ ->
            (model, Cmd.none)
        UpdateProof f val ->
            ({ model | proofView = model.proofView
                   |> \pv -> { pv | proof = f pv.proof val }
             }, Cmd.none)
        ToggleEditMode ->
            ({ model | proofView = model.proofView
                   |> \pv -> { pv | isEdited = not pv.isEdited }
             }, Cmd.none)
        RemoveProof ->
            let route = "/private/proof/delete/"
                        ++ String.fromInt model.proofView.proof.id
                cmd = delete model route RemovedProof
            in (model, cmd)
        RemovedProof (Ok pid) ->
            let pid_ = Maybe.withDefault (-1) <| String.toInt pid
            in ({ model | proofView = model.proofView
                |> \pv -> { pv | proof = defaultProof }
                , proofs = List.filter (\p -> p.id /= pid_) model.proofs
                }, fire <| SwitchPage ProjectBrowserPage)
        RemovedProof (Err e) ->
            errHandler model e
        SaveProof ->
            let cmd =
                    post model
                        ("/private/proof/update/"
                             ++ String.fromInt model.proofView.proof.id)
                        (encodeProof model.proofView.proof)
                        SavedProof
            in (model, cmd)
        SavedProof (Ok _) ->
            ({ model | proofView = model.proofView
                   |> \pv -> { pv | isEdited = False }
             }, Cmd.none)
        SavedProof (Err e) ->
            errHandler model e
        LoadComments ->
            let route = "/private/comment/list/"
                        ++ String.fromInt model.proofView.proof.id
                cmd = get model route [] LoadedComments
            in (model, cmd)
        LoadedComments (Ok res) ->
            let modelHelper ml cs =
                    { ml | proofView = ml.proofView
                    |> \pv -> { pv | comments = cs }
                    }
            in decodeHandler model (Decode.list commentDecoder) res modelHelper
        LoadedComments (Err e) ->
            errHandler model e
        UpdateNewComment f val ->
            ({ model | proofView = model.proofView
                   |> \pv -> { pv | newComment = f pv.newComment val
                             }
             }, Cmd.none)
        AddComment ->
            let newComment =
                    model.proofView.newComment
                        |> \c -> { c | userId = model.user.email
                                 , proofId = model.proofView.proof.id
                                 }
                cmd =
                    post model
                        "/private/comment/new/"
                        (encodeComment newComment)
                        AddedComment
                model_ = { model | proofView = model.proofView
                         |> \pv -> { pv | newComment = newComment }
                         }
            in (model_, cmd)
        AddedComment (Ok cid) ->
            let c_ = model.proofView.newComment
                   |> \c -> { c | id = Maybe.withDefault -1 <| String.toInt cid }
            in ({ model | proofView = model.proofView
                |> \pv -> { pv | comments = c_ :: pv.comments
                          , newComment = defaultComment
                          }
                }, Cmd.none)
        AddedComment (Err e) ->
            errHandler model e
        UpdateComment cid f val ->
            ({ model | proofView = model.proofView
                   |> \pv -> { pv | comments =
                                   List.Extra.updateIf
                                   (\c -> c.id == cid)
                                   (\c -> f c val)
                                   pv.comments
                             }
             }, Cmd.none)
        SaveComment cid ->
            let updComment =
                    model.proofView.comments
                           |> List.Extra.find (\c -> c.id == cid)
                           |> Maybe.withDefault defaultComment
                cmd = post model
                      ("/private/comment/update/" ++ String.fromInt cid)
                      (encodeComment updComment)
                          (SavedComment cid)
            in (model, cmd)
        SavedComment cid (Ok _) ->
            let model_ =
                    { model | proofView = model.proofView
                    |> \pv -> { pv | comments =
                                    List.Extra.updateIf
                                    (\c -> c.id == cid)
                                    (\c -> { c | isEdited = False })
                                    pv.comments
                              }
                    }
            in (model_, Cmd.none)
        SavedComment _ (Err e) ->
            errHandler model e
        RemoveComment cid ->
            let route = "/private/comment/delete/" ++ String.fromInt cid
                cmd = delete model route (RemovedComment cid)
            in (model, cmd)
        RemovedComment cid (Ok _) ->
            ({ model | proofView = model.proofView
                   |> \pv -> { pv | comments =
                                   List.filter
                                   (\c -> c.id /= cid)
                                   pv.comments
                             }
             }, Cmd.none)
        RemovedComment _ (Err e) ->
            errHandler model e

view : Model -> Html Message
view model =
    let editable = model.project.editable
        m = model.proofView
        leftCol =
            containerFluid
            [ row [ column
                        [ Medium Two ]
                        [ input [ type_ "button"
                                , value "Return"
                                , onClick <| SwitchPage ProjectBrowserPage
                                , class "form-control"
                                ] []
                        ]
                  ]
            , hr [] []
            , row [ column
                        [ Medium Eight ]
                        [ input [ value m.proof.title
                                , onInput <| UpdateProof (\p v -> { p | title = v })
                                , readonly <| not (editable && m.isEdited)
                                , class "form-control"
                                ] []
                        ]
                  , column
                      [ Medium Two ]
                      [ input [ type_ "button"
                              , value <| if m.isEdited
                                         then "Save"
                                         else "Edit"
                              , onClick <| if m.isEdited
                                           then SaveProof
                                           else ToggleEditMode
                              , disabled <| not editable
                              , class "form-control btn-info"
                              ] []
                      ]
                  , column
                      [ Medium Two ]
                      [ input [ type_ "button"
                              , value "Remove"
                              , onClick RemoveProof
                              , disabled <| not editable
                              , class "form-control btn-danger"
                              ] []
                      ]
                  ]
            , br [] []
            , row [ column [ Medium Twelve ]
                        [ input [ value m.proof.shortDescription
                                , onInput <| UpdateProof (\p v -> { p | shortDescription = v })
                                , readonly <| not (editable && m.isEdited)
                                , class "form-control"
                                ] []
                        ]
                  ]
            , br [] []
            , row [ column [ Medium Twelve ]
                        [ textarea [ onInput <| UpdateProof (\p v -> { p | longDescription = v })
                                   , readonly <| not (editable && m.isEdited)
                                   , class "form-control"
                                   , rows 6
                                   ] [ text m.proof.longDescription ]
                        ]
                        
                  ]
            , br [] []
            , row [ column [ Medium Twelve ]
                        [ textarea [ onInput <| UpdateProof (\p v -> { p | text = v })
                                   , readonly <| not (editable && m.isEdited)
                                   , style "height" "60vh"
                                   , class "form-control"
                                   ] [ text m.proof.text ]
                        ]
                  ]
            ]                        
        rightCol =
            let comments =
                    m.comments
                        |> List.map (\c ->
                                         containerFluid 
                                              [ row [ column
                                                          [ ExtraSmall Six, Small Six, Medium Six, Large Six ]
                                                          [ text <| c.userId ++ " wrote: " ]
                                                    , column
                                                          [ ExtraSmall Three, Small Three, Medium Three, Large Three ]
                                                          [ input [ type_ "button"
                                                                  , value <| if c.isEdited
                                                                             then "Save"
                                                                             else "Edit"
                                                                  , onClick <| if c.isEdited
                                                                               then SaveComment c.id
                                                                               else UpdateComment c.id (\x _ -> { x | isEdited = True }) ""
                                                                  , readonly <| not (c.userId == model.user.email && c.isEdited)
                                                                  , class "form-control"
                                                                  ] []
                                                          ]
                                                    , column
                                                          [ ExtraSmall Three, Small Three, Medium Three, Large Three ]
                                                          [ input [ type_ "button"
                                                                  , value "Delete"
                                                                  , onClick <| RemoveComment c.id
                                                                  , disabled <| not (c.userId == model.user.email && c.isEdited)
                                                                  , class "form-control"
                                                                  ] []
                                                          ]
                                                    ]
                                              , br [] []
                                              , row [ column
                                                          [ ExtraSmall Twelve, Small Twelve, Medium Twelve, Large Twelve ]
                                                          [ textarea [ onInput <| UpdateComment c.id <| \x v -> { x | text = v }
                                                                     , readonly <| not (c.userId == model.user.email && c.isEdited)
                                                                     , rows 3
                                                                     , class "form-control"
                                                                     ] [ text c.text ]
                                                          ]
                                                    ]
                                              , br [] []
                                              , br [] []
                                              ])
            in div []
                <| comments
                    ++ [ containerFluid
                             [ row
                               [ column [ ExtraSmall Twelve, Small Twelve, Medium Twelve, Large Twelve ]
                                     [ div [ class "input-group" ]                               
                                           [ textarea [ onInput <| UpdateNewComment <| \x v -> { x | text = v }
                                                      , class "form-control"
                                                      , style "height" "150px"
                                                   ] [  text m.newComment.text ]
                                           , div [ class "input-group-btn" ]
                                               [ button [ type_ "button"
                                                        , onClick AddComment
                                                        , style "height" "150px"
                                                        , style "width" "50px"
                                                        , class "form-control"
                                                        ] [ text "+" ]
                                               ]
                                           ]
                                     ]
                               ]
                             ]
                       ]
    in container
        [ row
          [ column [ ExtraSmall Six, Small Eight, Medium Eight, Large Eight ]
                [ leftCol ]
          , column [ ExtraSmall Six, Small Four, Medium Four, Large Four ]
                [ rightCol ]
          ]
        ]

init : Cmd Message
init =
    Cmd.batch [ fire LoadComments ]
