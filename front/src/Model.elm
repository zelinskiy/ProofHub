module Model exposing (..)

import Pages exposing (..)

type alias Model =
    { debug : String
    , openedPage : Page
    }

defaultModel : Model
defaultModel =
    { debug = ""
    , openedPage = LoginViewPage
    }
