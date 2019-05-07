module Utils exposing (..)

import Http

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

