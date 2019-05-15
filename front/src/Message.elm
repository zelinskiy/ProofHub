module Message exposing (..)

import LoginView
import Dashboard
import ProjectBrowser 
import ProofView
import ProjectView

import Time exposing (Posix)

type Message = LoginViewMessage LoginView.Message
             | DashboardMessage Dashboard.Message
             | ProjectBrowserMessage ProjectBrowser.Message
             | ProofViewMessage ProofView.Message
             | ProjectViewMessage ProjectView.Message
             | SetNow Posix
        
       
