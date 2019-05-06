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

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        LoginViewMessage (LoginView.SwitchPage p) ->
            ({ model | openedPage = p }, Cmd.none)
        DashboardMessage (Dashboard.SwitchPage p) ->
            ({ model | openedPage = p }, Cmd.none)
        ProjectBrowserMessage (ProjectBrowser.SwitchPage p) ->
            ({ model | openedPage = p }, Cmd.none)
        ProofViewMessage (ProofView.SwitchPage p) ->
            ({ model | openedPage = p }, Cmd.none)
                
        LoginViewMessage m ->
            Tuple.mapSecond (Cmd.map LoginViewMessage) <| LoginView.update m model
        DashboardMessage m ->
            Tuple.mapSecond (Cmd.map DashboardMessage) <| Dashboard.update m model
        ProjectBrowserMessage m ->
            Tuple.mapSecond (Cmd.map ProjectBrowserMessage) <| ProjectBrowser.update m model
        ProofViewMessage m ->
            Tuple.mapSecond (Cmd.map ProofViewMessage) <| ProofView.update m model

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
                 , div [] [ Markdown.toHtml [] model.debug ]
                 ]
        , update = update
        , subscriptions = \m -> Sub.batch []
        }
