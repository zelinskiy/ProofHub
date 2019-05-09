module Message exposing (..)

import LoginView
import Dashboard
import ProjectBrowser 
import ProofView
import ProjectView

type Message = LoginViewMessage LoginView.Message
             | DashboardMessage Dashboard.Message
             | ProjectBrowserMessage ProjectBrowser.Message
             | ProofViewMessage ProofView.Message
             | ProjectViewMessage ProjectView.Message
        
       
