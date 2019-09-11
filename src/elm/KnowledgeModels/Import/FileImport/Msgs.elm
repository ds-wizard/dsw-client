module KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import File exposing (File)
import KnowledgeModels.Common.Package exposing (Package)


type Msg
    = DragEnter
    | DragLeave
    | PickFiles
    | GotFiles File (List File)
    | Submit
    | Cancel
    | ImportPackageCompleted (Result ApiError (List Package))
