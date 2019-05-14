import Html exposing (..)
import Html.Attributes exposing (..)
import Browser
import Markdown

import Pages exposing (Page(..))
import Model exposing (..)
import Message exposing (..)
import LoginView
import Dashboard
import ProjectBrowser 
import ProofView
import ProjectView

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    let getInit p =
            case p of
                LoginViewPage ->
                    Cmd.map LoginViewMessage LoginView.init
                DashboardPage ->
                    Cmd.map DashboardMessage Dashboard.init
                ProjectBrowserPage ->
                    Cmd.map ProjectBrowserMessage ProjectBrowser.init
                ProofViewPage ->
                    Cmd.map ProofViewMessage ProofView.init
                ProjectViewPage ->
                    Cmd.map ProjectViewMessage ProjectView.init
    in case msg of
        LoginViewMessage (LoginView.SwitchPage p) ->
            ({ model | openedPage = p }, getInit p)
        DashboardMessage (Dashboard.SwitchPage p) ->
            ({ model | openedPage = p }, getInit p)
        ProjectBrowserMessage (ProjectBrowser.SwitchPage p) ->
            ({ model | openedPage = p }, getInit p)
        ProofViewMessage (ProofView.SwitchPage p) ->
            ({ model | openedPage = p }, getInit p)
        ProjectViewMessage (ProjectView.SwitchPage p) ->
            ({ model | openedPage = p }, getInit p)
                
        LoginViewMessage m ->
            Tuple.mapSecond (Cmd.map LoginViewMessage)
                <| LoginView.update m model
        DashboardMessage m ->
            Tuple.mapSecond (Cmd.map DashboardMessage)
                <| Dashboard.update m model
        ProjectBrowserMessage m ->
            Tuple.mapSecond (Cmd.map ProjectBrowserMessage)
                <| ProjectBrowser.update m model
        ProofViewMessage m ->
            Tuple.mapSecond (Cmd.map ProofViewMessage)
                <| ProofView.update m model
        ProjectViewMessage m ->
            Tuple.mapSecond (Cmd.map ProjectViewMessage)
                <| ProjectView.update m model

view : Model -> Html Message
view model =
    case model.openedPage of
        LoginViewPage ->
            Html.map LoginViewMessage <| LoginView.view model
        DashboardPage ->
            Html.map DashboardMessage <| Dashboard.view model
        ProjectBrowserPage ->
            Html.map ProjectBrowserMessage <| ProjectBrowser.view model
        ProofViewPage ->
            Html.map ProofViewMessage <| ProofView.view model
        ProjectViewPage ->
            Html.map ProjectViewMessage <| ProjectView.view model

main : Program () Model Message
main =
    let css path = Html.node "link" [ Html.Attributes.rel "stylesheet"
                                    , Html.Attributes.href path ] []
    in Browser.document
        { init = \_ -> (defaultModel, Cmd.none)
        , view = \model -> Browser.Document "ProofHub" [
                  view model
                 , css "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
                 , css "https://use.fontawesome.com/releases/v5.7.2/css/all.css"
                 , div [] [ Markdown.toHtml [] "" {-model.debug-} ]
                 ]
        , update = update
        , subscriptions = \m -> Sub.batch []
        }
