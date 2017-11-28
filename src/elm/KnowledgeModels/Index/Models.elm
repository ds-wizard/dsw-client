module KnowledgeModels.Index.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import KnowledgeModels.Models exposing (KnowledgeModel)


type alias Model =
    { knowledgeModels : ActionResult (List KnowledgeModel)
    , kmToBeDeleted : Maybe KnowledgeModel
    , deletingKnowledgeModel : ActionResult String
    , migrationUuid : Maybe String
    , creatingMigration : ActionResult String
    }


initialModel : Model
initialModel =
    { knowledgeModels = Loading
    , kmToBeDeleted = Nothing
    , deletingKnowledgeModel = Unset
    , migrationUuid = Nothing
    , creatingMigration = Unset
    }
