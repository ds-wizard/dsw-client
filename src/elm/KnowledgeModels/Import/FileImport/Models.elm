module KnowledgeModels.Import.FileImport.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import File exposing (File)


type alias Model =
    { hover : Bool
    , file : Maybe File
    , importing : ActionResult String
    }


initialModel : Model
initialModel =
    { hover = False
    , file = Nothing
    , importing = Unset
    }
