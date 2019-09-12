module SHACLEditor.Models exposing
    ( CurrentEditor(..)
    , Model
    , createQuestionnaireModel
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models
import KMEditor.Common.KnowledgeModel.KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KnowledgeModels.Common.Package as Package
import Questionnaires.Common.QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))


type alias Model =
    { currentEditor : CurrentEditor
    , shacl : String
    , questionnaire : ActionResult Common.Questionnaire.Models.Model
    , levels : ActionResult (List Level)
    }


type CurrentEditor
    = SHACLEditor
    | PreviewEditor


initialModel : Model
initialModel =
    { currentEditor = SHACLEditor
    , shacl = ""
    , questionnaire = Unset
    , levels = Loading
    }


createQuestionnaireModel : AppState -> ActionResult KnowledgeModel -> Model -> Model
createQuestionnaireModel appState kmResult model =
    let
        toQuestionnaire km =
            Common.Questionnaire.Models.initialModel
                appState
                { uuid = ""
                , name = ""
                , accessibility = PrivateQuestionnaire
                , ownerUuid = Nothing
                , package = Package.dummy
                , knowledgeModel = km
                , replies = []
                , level = 1
                , selectedTagUuids = []
                , labels = []
                }
                []
                []
    in
    { model | questionnaire = ActionResult.map toQuestionnaire kmResult }
