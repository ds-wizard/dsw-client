module Questionnaires.CreateMigration.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.KnowledgeModel.KnowledgeModel exposing (KnowledgeModel)
import KnowledgeModels.Common.Package exposing (Package)
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Common.QuestionnaireMigration exposing (QuestionnaireMigration)


type Msg
    = GetPackagesCompleted (Result ApiError (List Package))
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | FormMsg Form.Msg
    | SelectPackage String
    | PostMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
