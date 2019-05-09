module Model exposing (..)

import Settings exposing (settings)
import Pages exposing (..)

type alias LoginViewModel =
    { passConfirmation : String
    , mode : LoginViewMode
    }

defaultLoginViewModel : LoginViewModel
defaultLoginViewModel =
    { passConfirmation = "administrator1"
    , mode = LoginMode
    }

type UserRole
    = Normal
    | Moderator
    | Admin
    
type alias User =
    { email : String
    , password : String
    , role : UserRole
    , avatarPath : String
    }

defaultUser : User
defaultUser =
    { email = "admin" -- PK
    , password = "administrator1"
    , role = Normal
    , avatarPath =
        settings.server
            ++ "/static/avatars/default.png"
    }

type alias Project =
    { id : Int -- PK
    , title : String
    , added : Int
    , updated : Int
    , proverId : String -- FK
    , longDescription : String
    , shortDescription : String
    , categoriesTitles : List String
    , authorsEmails : List String
    }

defaultProject : Project
defaultProject =
    { id = -1 
    , title = "Project"
    , added = 0
    , updated = 0
    , proverId = "Agda 1.5.2" 
    , longDescription = "Long description"
    , shortDescription = "Short description"
    , categoriesTitles = []
    , authorsEmails = []
    }

type alias Directory =
    { id : Int -- PK
    , title : String
    , parentDirectoryId : Maybe String -- FK
    , projectId : Int -- FK
    }

defaultDirectory : Directory
defaultDirectory =
    { id = -1
    , title = "Directory"
    , parentDirectoryId = Nothing
    , projectId = -1
    }

type alias Proof =
    { id : Int -- PK
    , title : String
    , text : String
    , longDescription : String
    , shortDescription : String
    , directoryId : Int -- FK
    }

defaultProof : Proof
defaultProof =
    { id = -1
    , title = "Proof.agda"
    , text = "Code goes here"
    , longDescription = "Long description"
    , shortDescription = "Short description"
    , directoryId = -1
    }

type alias Category =
    { title : String -- PK
    , description : String
    }

defaultCategory : Category
defaultCategory =
    { title = "Category"
    , description = "Category description"
    }

type alias Prover =
    { title : String -- PK
    }

defaultProver : Prover
defaultProver =
    { title = "Prover 1.0"
    }

type alias Comment =
    { id : Int -- PK
    , text : String
    , proofId : Int -- FK
    , userId : String -- FK
    }

defaultComment : Comment
defaultComment =
    { id = -1
    , text = "Comment"
    , proofId = -1
    , userId = ""
    }
                          
             
type alias Model =
    { debug : String
    , openedPage : Page
    , loginView : LoginViewModel
    , jwtToken : String
    , user : User
    , users : List User
    , projects : List Project
    , directories : List Directory
    , proofs : List Proof
    , provers : List Prover
    , categories : List Category
    , comments : List Comment
    }

defaultModel : Model
defaultModel =
    { debug = ""
    , openedPage = LoginViewPage
    , loginView = defaultLoginViewModel
    , jwtToken = ""
    , user = defaultUser
    , users = []
    , projects = []
    , directories = []
    , proofs = []
    , provers = []
    , categories = []
    , comments = []
    }
