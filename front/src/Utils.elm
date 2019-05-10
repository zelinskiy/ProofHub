module Utils exposing (..)

import Settings exposing (settings)
import Model exposing (..)

import Process
import Task
import Http
import Json.Encode as Encode
import Json.Decode as Decode

-- "Lenses"
updLoginView model f =
    { model | loginView = f model.loginView }
updUser model f =
    { model | user = f model.user }
updProject model f =
    { model | project = f model.project }
updProjectView model f =
    { model | projectView = f model.projectView }
updDashboard model f =
    { model | dashboard = f model.dashboard }
        

errorToString : Http.Error -> String
errorToString error =
    case error of
         Http.BadUrl e ->
             "Bad url " ++ e
         Http.Timeout ->
             "Timeout"
         Http.NetworkError ->
             "Network error"
         Http.BadStatus code ->
             "Bad status " ++ String.fromInt code
         Http.BadBody e ->
             "Bad body " ++ e

post : Model
     -> String
     -> Encode.Value
     -> (Result Http.Error String -> msg)
     -> Cmd msg
post model route data resMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ model.jwtToken ]
        , url = settings.server ++ route
        , body = Http.jsonBody data
        , expect = Http.expectString resMsg
        , timeout = Nothing
        , tracker = Nothing
        }

get : Model
    -> String
    -> List (String, String)
    -> (Result Http.Error String -> msg)
    -> Cmd msg
get model route params resMsg =
    let helper (n, v) acc =
            acc ++ "&" ++ n ++ "=" ++ v
        params_ =
            List.foldl helper "" params
    in Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ model.jwtToken ]
        , url = settings.server ++ route
        , body = Http.emptyBody 
        , expect = Http.expectString resMsg
        , timeout = Nothing
        , tracker = Nothing
        }

delete : Model
     -> String
     -> (Result Http.Error String -> msg)
     -> Cmd msg
delete model route resMsg =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ model.jwtToken ]
        , url = settings.server ++ route
        , body = Http.emptyBody
        , expect = Http.expectString resMsg
        , timeout = Nothing
        , tracker = Nothing
        }
        
delay : Float -> msg -> Cmd msg
delay n msg =
    Process.sleep n |> Task.perform (\_ -> msg)

fire : msg -> Cmd msg
fire msg =
    Task.succeed () |> Task.perform (\_ -> msg)
        
-- No explicit type declaration to avoid loop imports
errHandler model e =
    ( { model | debug = errorToString e }, Cmd.none)

decodeHandler m decoder str f = 
    let m_ =
            case Decode.decodeString decoder str of
                Ok res -> f m res
                Err e -> { m | debug = Decode.errorToString e }
    in (m_, Cmd.none)
        
timeNow : Model -> String
timeNow _ = ""
