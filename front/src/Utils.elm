module Utils exposing (..)

import Http

-- "Lenses"
updLoginView model f =
    { model | loginView = f model.loginView }
updUser model f =
    { model | user = f model.user }

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

