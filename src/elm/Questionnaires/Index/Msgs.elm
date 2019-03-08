module Questionnaires.Index.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.ExportModal.Msgs as ExportModal
import KnowledgeModels.Common.Models exposing (PackageDetail)


type Msg
    = GetQuestionnairesCompleted (Result ApiError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result ApiError ())
    | ShowExportQuestionnaire Questionnaire
    | ExportModalMsg ExportModal.Msg
    | ShowHideQuestionnaireUpgradeForm (Maybe Questionnaire)
    | DeleteQuestionnaireMigration String
    | DeleteQuestionnaireMigrationCompleted (Result ApiError ())
    | UpgradeQuestionnaireForm Form.Msg
    | GetUpgradableKnowledgeModelsCompleted (Result ApiError (List PackageDetail))
    | PostQuestionnaireMigrationCompleted (Result ApiError ())
