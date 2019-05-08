module Api.File (API, server) where

import Servant
import Servant.Multipart
import System.Directory
import Control.Monad.IO.Class

type API =
     MultipartForm Tmp (MultipartData Tmp)
  :> QueryParam "filePath" String
  :> Post '[PlainText] String

server :: MultipartData Tmp -> Maybe String -> Handler String
server _ Nothing =
  throwError $ err403
  { errBody = "filePath parameter required" } 
server multipartData (Just filePath) = do
  let mbFilePath = fdPayload <$> lookupFile "file" multipartData
  case mbFilePath of
    Nothing ->
      throwError $ err403
      { errBody = "file form field required" }        
    Just fp -> do
      let fp_ = "static/" ++ filePath
      liftIO $ copyFile fp fp_
      return fp_
        
