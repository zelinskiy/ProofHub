module Model exposing (..)

import Pages exposing (..)

type alias Model =
    { debug : String
    , openedPage : Page
    , userEmail : String
    , userPassword : String
    , passwordConfirmation : String
    }

defaultModel : Model
defaultModel =
    { debug = ""
    , openedPage = DashboardPage -- LoginViewPage LoginMode
    , userEmail = "admin"
    , userPassword = "administrator1"
    , passwordConfirmation = "administrator1"
    }
