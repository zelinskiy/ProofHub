module Model exposing (..)

import Settings exposing (settings)
import Pages exposing (..)
import Time exposing (..)

type alias LoginViewModel =
    { passConfirmation : String
    , mode : LoginViewMode
    }

defaultLoginViewModel : LoginViewModel
defaultLoginViewModel =
    { passConfirmation = "administrator1"
    , mode = LoginMode
    }

type alias ProjectViewModel =
    { newAuthor : String
    }

defaultProjectViewModel : ProjectViewModel
defaultProjectViewModel =
    { newAuthor = ""
    }

type alias DashboardModel =
    { newProver : Prover
    , newCategory : Category
    , editingUser : Bool
    , selectedProversTitles : List String
    , selectedCategoriesTitles : List String
    , selectedUserEmails : List String
    , queryText : String
    , currentPage : Int
    , projectsOnPage : Int
    , selectedUser : String
    }

defaultDashboardModel : DashboardModel
defaultDashboardModel  =
    { newProver = defaultProver
    , newCategory = defaultCategory
    , editingUser = False
    , selectedProversTitles = []
    , selectedCategoriesTitles = []
    , selectedUserEmails = []
    , currentPage = 0
    , projectsOnPage = 10
    , queryText = ""
    , selectedUser = ""
    }

type alias ProofViewModel =
    { proof : Proof
    , comments : List Comment
    , isEdited : Bool
    , newComment : Comment
    }

defaultProofViewModel : ProofViewModel
defaultProofViewModel =
    { proof = defaultProof
    , comments = []
    , isEdited = False
    , newComment = defaultComment
    }

type alias ProjectBrowserModel =
    { directory : Maybe Directory
    }

defaultProjectBrowserModel : ProjectBrowserModel
defaultProjectBrowserModel =
    { directory = Nothing
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

type alias ExtProject =
    { project : Project
    , categories : List String
    , authors : List String
    }
    
type alias Project =
    { id : Int -- PK
    , title : String
    , added : String -- UTC format
    , updated : String -- UTC format
    , proverId : String -- FK
    , longDescription : String
    , shortDescription : String
    , categoriesTitles : List String
    , authorsEmails : List String
    , editable : Bool
    }

defaultProject : Project
defaultProject =
    { id = -1 
    , title = "Project"
    , added = "2019-05-10T13:50:14Z"
    , updated = "2019-05-10 13:50:14Z"
    , proverId = "Agda 1.0" 
    , longDescription = "Long description"
    , shortDescription = "Short description"
    , categoriesTitles = []
    , authorsEmails = []
    , editable = True
    }



type alias Directory =
    { id : Int -- PK
    , title : String
    , parentDirectoryId : Maybe Int -- FK
    , projectId : Int -- FK
    , isEdited : Bool -- Local
    }

defaultDirectory : Directory
defaultDirectory =
    { id = -1
    , title = "Directory"
    , parentDirectoryId = Nothing
    , projectId = -1
    , isEdited = False
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
    , isEdited : Bool -- Stored locally
    }

defaultComment : Comment
defaultComment =
    { id = -1
    , text = "Comment"
    , proofId = -1
    , userId = ""
    , isEdited = False
    }
                          
             
type alias Model =
    { debug : String
    , openedPage : Page
    , loginView : LoginViewModel
    , projectView : ProjectViewModel
    , dashboard : DashboardModel
    , projectBrowser : ProjectBrowserModel
    , proofView : ProofViewModel
    , jwtToken : String
    , user : User
    , users : List User
    , projects : List Project
    , project : Project
    , directories : List Directory
    , proofs : List Proof
    , provers : List Prover
    , categories : List Category
    , zone : Zone
    , now : Posix
    }

defaultModel : Model
defaultModel =
    { debug = ""
    , openedPage = LoginViewPage
    , loginView = defaultLoginViewModel
    , projectView = defaultProjectViewModel
    , dashboard = defaultDashboardModel
    , projectBrowser = defaultProjectBrowserModel
    , proofView = defaultProofViewModel
    , jwtToken = ""
    , user = defaultUser
    , project = defaultProject
    , users = []
    , projects = []
    , directories = []
    , proofs = []
    , provers = []
    , categories = []
    , now = Time.millisToPosix 0
    , zone = utc  
    }
