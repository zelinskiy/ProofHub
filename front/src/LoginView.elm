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
    case msg of
        UpdateSomething val ->
            (model, Cmd.none)
        SwitchPage _ ->
            -- Leave it empty - this is handled on the higher level
            (model, Cmd.none)
        SwitchMode m ->
            ({ model | openedPage = LoginViewPage m }, Cmd.none)
        UpdateUserEmail val ->
            ({ model | userEmail = val }, Cmd.none)
        UpdateUserPassword val ->
            ({ model | userPassword = val }, Cmd.none)
        UpdatePasswordConfirmation val ->
            ({ model | passwordConfirmation = val }, Cmd.none)            
        Register ->
            let body = Json.object [ ("email", Json.string model.userEmail)
                                   , ("pass", Json.string model.userPassword)
                                   ]
                cmd = Http.request
                      { method = "POST"
                      , headers = []
                      , url = settings.server ++ "/public/user/register"
                      , body = Http.jsonBody body
                      , expect = Http.expectString Registered
                      , timeout = Nothing
                      , tracker = Nothing
                      }
            in if model.userPassword /= model.passwordConfirmation
               then ({ model | debug = "Passwords do not match" }, Cmd.none)
               else if String.length model.userPassword < 7
                    then ({ model | debug = "Password must be at least 7 characters long" }, Cmd.none)
                    else (model, cmd)
        Registered (Ok res) ->
            ({ model | debug = res }, Cmd.none)
        Registered (Err e) ->
            ({ model | debug = errorToString e }, Cmd.none)
        Login ->
            let body = Json.object [ ("email", Json.string model.userEmail)
                                   , ("pass", Json.string model.userPassword)
                                   ]
                cmd = Http.request
                      { method = "POST"
                      , headers = []
                      , url = settings.server ++ "/public/jwt/login"
                      , body = Http.jsonBody body
                      , expect = Http.expectString Logged
                      , timeout = Nothing
                      , tracker = Nothing
                      }
            -- in (model, cmd)
            in (model, Cmd.batch [ Process.sleep 50
                                 |> Task.perform (\_ -> SwitchPage DashboardPage)
                                 ])
        Logged (Ok res) ->
            ({ model | debug = res }, Cmd.none)
        Logged (Err e) ->
            ({ model | debug = errorToString e }, Cmd.none)

view : Model -> Html Message
view model =
    let variativePart =
            case model.openedPage of
                LoginViewPage LoginMode ->
                    [ button [ onClick Login ] [ text "Login" ]
                    , button [ onClick <| SwitchMode RegisterMode ] [ text "Go to register" ]
                    ]
                LoginViewPage RegisterMode ->
                    [ input [ onInput UpdatePasswordConfirmation, value model.passwordConfirmation ] []
                    , button [ onClick Register ] [ text "Register" ]
                    , button [ onClick <| SwitchMode LoginMode ] [ text "Go to Login" ]
                    ]
                _ -> []
    in div [] <| [ input [ onInput UpdateUserEmail, value model.userEmail ] []
                 , input [ onInput UpdateUserPassword, value model.userPassword ] []
                 ] ++ variativePart
