module Questionnaires.Migration.Models exposing
    ( Model
    , initialModel
    , DiffState(..)
    , DiffNode(..)
    , KnowledgeModelDiffNodeData
    , ChapterDiffNodeData
    , QuestionDiffNodeData
    , AnswerDiffNodeData
    , StringDiff
    , TreeNode
    , TreeNodeType(..)
    , ExpansionState(..)
    , NodeUuids
    , stateNodes
    , getNodeUuid
    , getDiffStateUuid
    , hasPreviousDiffEvent
    , hasFollowingDiffEvent
    , getParentQuestionPathForUuid
    , isUnchangedState
    , stringifyPath
    )

import Diff
import Dict exposing (Dict)
import ActionResult exposing (ActionResult(..))
import SplitPane exposing (Orientation(..), configureSplitter, percentage)
import FormEngine.Model exposing (ReplyValue)
import Questionnaires.Common.Models exposing (QuestionnaireMigration)
import Common.Questionnaire.Models exposing (QuestionFlags)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Chapter, Question, Answer, FollowUps(..), getQuestionUuid, getQuestionTitle, getQuestionAnswers)
import KMEditor.Common.Models.Events exposing (Event(..), getEventEntityUuid)
import KMEditor.Common.Models.Path as Path
import List.Extra


type alias Model =
    { questionnaireMigration : ActionResult QuestionnaireMigration
    , settingQuestionFlag : ActionResult String
    , removingQuestionFlag : ActionResult String
    , splitPaneState : SplitPane.State
    , activeNode : NodeUuids
    , diffStates : Maybe (Dict String DiffState)
    , diffTree : Maybe (Dict String TreeNode)
    , diffEventsUuids : List NodeUuids
    , questionFlagToBeAdded : Maybe QuestionFlags
    , questionFlagToBeRemovedPath : Maybe (List String)
    }


type DiffState
    = Created DiffNode
    | Removed DiffNode
    | Unchanged DiffNode
    | Modified DiffNode DiffNode


type DiffNode
    = KnowledgeModelDiffNode KnowledgeModelDiffNodeData
    | ChapterDiffNode ChapterDiffNodeData
    | QuestionDiffNode QuestionDiffNodeData
    | AnswerDiffNode AnswerDiffNodeData


type alias KnowledgeModelDiffNodeData =
    { uuid : String
    , name : StringDiff
    }


type alias ChapterDiffNodeData =
    { uuid : String
    , title : StringDiff
    , text : StringDiff
    }


type alias QuestionDiffNodeData =
    { uuid : String
    , title : StringDiff
    , text : StringDiff
    , question : Question
    }

type alias AnswerDiffNodeData =
    { uuid : String
    , label : StringDiff
    , advice : StringDiff
    , answer : Answer
    }


type alias TreeNode =
    { uuid : String
    , treeUuid : String -- Adds 0., 1. etc. for answered questions
    , children : List String
    , title : String
    , expansionState : ExpansionState
    , path : Path.Path
    , nodeType : TreeNodeType
    }


type TreeNodeType
    = KnowledgeModelType
    | ChapterType
    | QuestionType (Maybe ReplyValue)
    | QuestionListItemType
    | AnswerType


type ExpansionState
    = Expanded
    | Collapsed


type alias NodeUuids =
    { treeUuid : String
    , diffStateUuid : String
    }


type alias StringDiff = List (Diff.Change String)


initialModel : Model
initialModel =
    { questionnaireMigration = Loading
    , settingQuestionFlag = Unset
    , removingQuestionFlag = Unset
    , splitPaneState = initialSplitPaneState
    , activeNode = { treeUuid = "", diffStateUuid = "" }
    , diffStates = Nothing
    , diffTree = Nothing
    , diffEventsUuids = []
    , questionFlagToBeAdded = Nothing
    , questionFlagToBeRemovedPath = Nothing
    }


containsTreeUuid : List NodeUuids -> String -> Bool
containsTreeUuid uuids uuid =
    List.any (\node -> node.treeUuid == uuid) uuids


containsDiffStateUuid : List NodeUuids -> String -> Bool
containsDiffStateUuid uuids uuid =
    List.any (\node -> node.diffStateUuid == uuid) uuids


diffStrings : String -> String -> StringDiff
diffStrings l r =
    Diff.diff (String.toList l) (String.toList r)
        |> List.map (mapChange <| String.fromChar)


mapChange : (a -> b) -> Diff.Change a -> Diff.Change b
mapChange f change =
    case change of
        Diff.Added c -> Diff.Added <| f c
        Diff.Removed c -> Diff.Removed <| f c
        Diff.NoChange c -> Diff.NoChange <| f c


stateNodes : DiffState -> ( Maybe DiffNode, Maybe DiffNode )
stateNodes state =
    case state of
        Created node -> ( Nothing, Just node )
        Removed node -> ( Just node, Nothing )
        Unchanged node -> ( Just node, Just node )
        Modified oldNode newNode -> ( Just oldNode, Just newNode )


getNodeUuid : DiffNode -> String
getNodeUuid node =
    case node of
        KnowledgeModelDiffNode knowledgeModelDiffNode ->
            knowledgeModelDiffNode.uuid

        ChapterDiffNode chapterDiffNode ->
            chapterDiffNode.uuid

        QuestionDiffNode questionDiffNode ->
            questionDiffNode.uuid

        AnswerDiffNode answerDiffNode ->
            answerDiffNode.uuid


hasFollowingDiffEvent : Model -> NodeUuids -> Bool
hasFollowingDiffEvent model uuid =
    let
        isMember =
            List.member uuid model.diffEventsUuids

        isLastEvent =
            isLast uuid model.diffEventsUuids
    in
    isMember && (not isLastEvent)


hasPreviousDiffEvent : Model -> NodeUuids -> Bool
hasPreviousDiffEvent model uuid =
    let
        isMember =
            List.member uuid model.diffEventsUuids

        isFirstEvent =
            isFirst uuid model.diffEventsUuids
    in
    isMember && (not isFirstEvent)


isFirst : a -> List a -> Bool
isFirst element list =
    case List.head list of
        Just first ->
            first == element

        Nothing ->
            False

isLast : a -> List a -> Bool
isLast element list =
    case List.Extra.last list of
        Just last ->
            last == element

        Nothing ->
            False


isUnchangedState : DiffState -> Bool
isUnchangedState state =
    case state of
        Unchanged _ ->
            True

        _ ->
            False


getDiffStateUuid : DiffState -> String
getDiffStateUuid state =
    case state of
        Created node ->
            getNodeUuid node

        Removed node ->
            getNodeUuid node

        Unchanged node ->
            getNodeUuid node

        Modified node _ ->
            getNodeUuid node


initialSplitPaneState : SplitPane.State
initialSplitPaneState =
    SplitPane.init Horizontal
        |> configureSplitter (percentage 0.2 (Just (0.05, 0.7)))

getParentQuestionPathForUuid : String -> Dict.Dict String TreeNode -> Maybe (List String)
getParentQuestionPathForUuid uuid diffTree =
    let
        getDiffNodeQuestionPath : TreeNode -> Maybe (List String)
        getDiffNodeQuestionPath node =
            case node.nodeType of
                KnowledgeModelType ->
                    Nothing

                ChapterType ->
                    Nothing

                QuestionType _ ->
                    convertToUuid node.path

                AnswerType ->
                    List.Extra.init node.path
                        |> Maybe.withDefault []
                        |> convertToUuid

                QuestionListItemType ->
                    Nothing

        convertToUuid path =
            List.map Path.getNodeUuid path
                |> Just
    in
    Dict.get uuid diffTree
        |> Maybe.andThen getDiffNodeQuestionPath


stringifyPath : Path.Path -> String
stringifyPath path =
    List.map Path.getNodeUuid path
        |> String.join "."
