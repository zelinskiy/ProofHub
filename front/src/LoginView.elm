module LoginView exposing (Message(..), update, view)

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
            let cmd = Http.request
                      { method = "POST"
                      , headers = []
                      , url = settings.server ++ "/public/user/register"
                      , body = Http.jsonBody loginData
                      , expect = Http.expectString Registered
                      , timeout = Nothing
                      , tracker = Nothing
                      }
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
            let cmd = Http.request
                      { method = "POST"
                      , headers = []
                      , url = settings.server ++ "/public/jwt/login"
                      , body = Http.jsonBody loginData
                      , expect = Http.expectString Logged
                      , timeout = Nothing
                      , tracker = Nothing
                      }
            in (model, cmd)
        Logged (Ok res) ->
            let cmd =
                    Process.sleep 50
                    |> Task.perform (\_ -> SwitchPage DashboardPage)
                jwt = String.dropRight 1 <| String.dropLeft 1 res
            in ({ model | debug = jwt, jwtToken = jwt }, Cmd.batch [ cmd ])
        Logged (Err e) ->
            ({ model | debug = errorToString e }, Cmd.none)

view : Model -> Html Message
view model =
    let variativePart =
            case model.loginView.mode of
                LoginMode ->
                    [ button
                          [ onClick Login ]
                          [ text "Login" ]
                    , button
                          [ onClick <| SwitchMode RegisterMode ]
                          [ text "Go to register" ]
                    ]
                RegisterMode ->
                    [ input [ onInput UpdatePasswordConfirmation
                            , value model.loginView.passConfirmation ] []
                    , button [ onClick Register ] [ text "Register" ]
                    , button [ onClick <| SwitchMode LoginMode ] [ text "Go to Login" ]
                    ]
    in div [] <| [ input [ onInput UpdateUserEmail
                         , value model.user.email ] []
                 , input [ onInput UpdateUserPassword
                         , value model.user.password ] []
                 ] ++ variativePart
