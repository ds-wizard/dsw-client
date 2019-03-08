module Questionnaires.Index.Models exposing (Model, QuestionnaireRow, QuestionnaireUpgradeForm, initQuestionnaireRow, initialModel, encodeQuestionnaireUpgradeForm, questionnaireUpgradeFormValidation)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Common.Form exposing (CustomFormError)
import Json.Encode as Encode exposing (..)
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.ExportModal.Models as ExportModal
import KnowledgeModels.Common.Models exposing (PackageDetail)


type alias Model =
    { questionnaires : ActionResult (List Questionnaire)
    , questionnaireToBeDeleted : Maybe Questionnaire
    , deletingQuestionnaire : ActionResult String
    , exportModalModel : ExportModal.Model
    , questionnaireToBeUpgraded : Maybe Questionnaire
    , creatingQuestionnaireMigration : ActionResult String
    , upgradableKnowledgeModels : ActionResult (List PackageDetail)
    , questionnaireUpgradeForm : Form CustomFormError QuestionnaireUpgradeForm
    , deletingMigration : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaires = Loading
    , questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    , exportModalModel = ExportModal.initialModel
    , questionnaireToBeUpgraded = Nothing
    , creatingQuestionnaireMigration = Unset
    , upgradableKnowledgeModels = Unset
    , questionnaireUpgradeForm = initQuestionnaireUpgradeForm
    , deletingMigration = Unset
    }


type alias QuestionnaireRow =
    { dropdownState : Dropdown.State
    , questionnaire : Questionnaire
    }

type alias QuestionnaireUpgradeForm =
    { targetPackageId : String
    }


initQuestionnaireRow : Questionnaire -> QuestionnaireRow
initQuestionnaireRow =
    QuestionnaireRow Dropdown.initialState

initQuestionnaireUpgradeForm : Form CustomFormError QuestionnaireUpgradeForm
initQuestionnaireUpgradeForm =
    Form.initial [] questionnaireUpgradeFormValidation

questionnaireUpgradeFormValidation : Validation CustomFormError QuestionnaireUpgradeForm
questionnaireUpgradeFormValidation =
    Validate.map QuestionnaireUpgradeForm
        (Validate.field "targetPackageId" Validate.string)

encodeQuestionnaireUpgradeForm : QuestionnaireUpgradeForm -> Encode.Value
encodeQuestionnaireUpgradeForm form =
    Encode.object
        [ ( "targetPackageId", Encode.string form.targetPackageId )]
