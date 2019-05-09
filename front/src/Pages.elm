module Pages exposing (Page(..), LoginViewMode(..))

type LoginViewMode = LoginMode
                   | RegisterMode

type Page = LoginViewPage
          | DashboardPage
          | ProjectBrowserPage
          | ProofViewPage
          | ProjectViewPage
