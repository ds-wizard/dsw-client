module Questionnaires.Migration.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Questionnaires.Common.Models exposing (QuestionnaireMigration)
import Questionnaires.Migration.Models exposing (TreeNode, NodeUuids)
import SplitPane


type Msg
    = GetQuestionnaireMigrationCompleted (Result ApiError QuestionnaireMigration)
    | SplitPaneMsg SplitPane.Msg
    | ToggleDiffTreeNode String
    | SelectDiffTreeNode TreeNode
    | ShowNextDiffEvent NodeUuids
    | ShowPreviousDiffEvent NodeUuids
    | SetMigrationChangeAsNeedsReview String (List String)
    | SetMigrationChangeAsResolved String (List String)
    | SettingMigrationChangeCompleted (Result ApiError ())
    | DeleteMigrationChange String (List String)
    | DeleteMigrationChangeCompleted (Result ApiError ())
    | MigrateQuestionnaire String
    | MigrateQuestionnaireCompleted (Result ApiError ())
