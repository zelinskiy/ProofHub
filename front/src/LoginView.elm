module LoginView exposing (Message(..), update, view, init)

import Model exposing (Model)
import Pages exposing (..)
import Settings exposing (..)
import Utils exposing (..)

import Html exposing (..) 
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Process
import Task
import Json.Encode as Json
import Bootstrap.Forms exposing (..)
import Bootstrap.Wells exposing (..)
import Bootstrap.Buttons exposing (..)
import Bootstrap.Grid exposing (..)

type Message
    = UpdateSomething String
    | SwitchPage Page
    | SwitchMode LoginViewMode
    | UpdateUserEmail String
    | UpdateUserPassword String
    | UpdatePasswordConfirmation String
    | Register
    | Registered (Result Http.Error String)
    | Login
    | Logged (Result Http.Error String)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    let loginData =
            Json.object [ ("email", Json.string model.user.email)
                        , ("pass", Json.string model.user.password)
                        ]
    in case msg of
        UpdateSomething val ->
            (model, Cmd.none)
        SwitchPage _ ->
            -- Leave it empty - this is handled on the higher level
            (model, Cmd.none)
        SwitchMode m ->
            (updLoginView model
                 <| \l -> { l | mode = m }
            , Cmd.none)
        UpdateUserEmail val ->
            (updUser model
                 <| \u -> { u | email = val }
            , Cmd.none)
        UpdateUserPassword val ->
            (updUser model
                 <| \u -> { u | password = val }
            , Cmd.none)
        UpdatePasswordConfirmation val ->
            (updLoginView model
                 <| \l -> { l | passConfirmation = val }
            , Cmd.none)            
        Register ->
            let cmd = post model "/public/user/register" loginData Registered
            in if model.user.password /= model.loginView.passConfirmation
               then ({ model | debug = "Passwords do not match" }, Cmd.none)
               else if String.length model.user.password < 7
                    then ({ model | debug = "Password must be at least 7 characters long" }, Cmd.none)
                    else (model, cmd)
        Registered (Ok res) ->
            ({ model | debug = res }, Cmd.none)
        Registered (Err e) ->
            ({ model | debug = errorToString e }, Cmd.none)
        Login ->
            let cmd = post model "/public/jwt/login" loginData Logged
            in (model, cmd)
        Logged (Ok res) ->
            let cmd = delay 50 <| SwitchPage DashboardPage
                jwt = String.dropRight 1 <| String.dropLeft 1 res
            in ({ model | debug = jwt, jwtToken = jwt }, Cmd.batch [ cmd ])
        Logged (Err e) ->
            ({ model | debug = errorToString e }, Cmd.none)

view : Model -> Html Message
view model =
    let variativePart =
            case model.loginView.mode of
                LoginMode ->
                    [ containerFluid
                          [ row
                            [ column [ Medium Six ]
                                  [ btn BtnPrimary
                                        [ BtnBlock, BtnSmall ]
                                        []
                                        [ onClick Login ]
                                        [ text "Login" ]
                                  ]
                            , column [ Medium Six ]
                                [ btn BtnDefault
                                    [ BtnBlock, BtnSmall ]
                                    []
                                    [ onClick <| SwitchMode RegisterMode ]
                                    [ text "Go to register" ]
                                ]
                            ]
                          ]
                    ]
                RegisterMode ->
                    [ formInput [ onInput UpdatePasswordConfirmation
                                , value model.loginView.passConfirmation ] []
                    , br [] []
                    , containerFluid
                          [ row
                            [ column [ Medium Six ]
                                  [ btn BtnPrimary
                                        [ BtnBlock, BtnSmall ]
                                        []
                                        [ onClick Register ]
                                        [ text "Register" ]
                                  ]
                            , column [ Medium Six ]
                                [ btn BtnDefault
                                    [ BtnBlock, BtnSmall ]
                                    []
                                    [ onClick <| SwitchMode LoginMode ]
                                    [ text "Go to login" ]
                                ]
                            ]
                          ]
                    ]
    in div [ style "width" "100vw"
           , style "height" "60vh"           
           , style "background-size" "contain"
           , style "background-image" <| 
               "url(\"" ++ settings.server ++ "/static/img/back.png"
                   ++ "\")"
           ] [ div [ style "padding-top" "15vh" ]
                   [ container
                         [ row
                           [ column  [ Small Four, Medium Four, Large Four ] []
                           , column  [ Small Four, Medium Four, Large Four ] [
                                  well WellLarge [] [
                                       div [] <| 
                                           [ formInput [ value model.user.email
                                                       , onInput UpdateUserEmail ] []
                                           , br [] [] 
                                           , formInput [ onInput UpdateUserPassword
                                                       , value model.user.password ] []
                                           , br [] []
                                           ] ++ variativePart      
                                      ]
                                      
                                 ]
                           , column  [ Small Four, Medium Four , Large Four ] []
                           ]
                         ]
                   ]
             ]
        
            
init : Cmd Message
init =
    Cmd.batch []
