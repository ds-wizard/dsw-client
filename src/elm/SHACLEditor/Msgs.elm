module SHACLEditor.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Msgs
import KMEditor.Common.KnowledgeModel.KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)


type Msg
    = OpenSHACLEditor
    | OpenPreviewEditor
    | ChangeSHACL String
    | GetPreviewCompleted (Result ApiError KnowledgeModel)
    | GetLevelsCompleted (Result ApiError (List Level))
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
