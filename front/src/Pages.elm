module Pages exposing (Page(..), LoginViewMode(..))

type LoginViewMode = LoginMode
                   | RegisterMode

type Page = LoginViewPage LoginViewMode
          | DashboardPage
          | ProjectBrowserPage
          | ProofViewPage
