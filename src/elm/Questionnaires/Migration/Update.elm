module Questionnaires.Migration.Update exposing (appendUnchagnedChapterNodes, appendUnchangedAnswerNodes, appendUnchangedKnowledgeModelNodes, appendUnchangedQuestionNodes, convertDiffEventToDiffState, createAnswerCreatedState, createAnswerDeletedState, createAnswerDiffSubTree, createAnswerModifiedState, createAnswerUnchangedState, createChapterCreatedState, createChapterDiffSubTree, createChapterModifiedState, createChapterRemovedState, createChapterUnchangedState, createDiffEventUuids, createDiffTree, createKnowledgeModelCreatedState, createKnowledgeModelModifiedState, createKnowledgeModelUnchangedState, createListItemQuestionDiffSubTree, createQuestionCreatedState, createQuestionDiffSubTree, createQuestionDiffSubTreeAtKey, createQuestionModifiedState, createQuestionRemovedState, createQuestionUnchangedState, diffStrings, dummyAnswer, dummyChapter, dummyQuestion, fetchData, findAnswerByUuid, findChapterByUuid, findQuestionByUuid, findQuestionReply, getChapterQuestions, getFollowUpQuestions, getKnowledgeModelAnswers, getKnowledgeModelQuestions, handleDeleteMigrationChange, handleDeleteMigrationChangeCompleted, handleGetQuestionnaireMigrationCompleted, handleMigrateQuestionnaire, handleMigrateQuestionnaireCompleted, handleNextDiffEvent, handleSelectDiffTreeNode, handleSetMigrationChangeAsNeedsReview, handleSetMigrationChangeAsResolved, handleSetMigrationQuestionFlag, handleSettingMigrationChangeCompleted, handleShowPreviousDiffEvent, handleToggleDiffTreeNode, insertDiffStateIfNotExists, mapDiffChange, newNodeDiffChanges, oldNodeDiffChanges, toggleDiffTreeNodeExpansionStateByUuid, toggleExpansionState, unwrapWithNestedQuestions, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (QuestionFlagType(..), QuestionFlags, encodeQuestionFlag, getReply)
import Dict exposing (Dict)
import Diff
import FormEngine.Model exposing (FormValues, ReplyValue(..), getAnswerUuid, getStringReply)
import KMEditor.Common.Models.Entities exposing (Answer, Chapter, FollowUps(..), KnowledgeModel, Question(..), getQuestionAnswers, getQuestionItemQuestions, getQuestionText, getQuestionTitle, getQuestionUuid)
import KMEditor.Common.Models.Events exposing (Event(..), getAddQuestionUuid, getEditQuestionUuid)
import KMEditor.Common.Models.Path exposing (Path, PathNode(..), getNodeUuid)
import List.Extra exposing (andThen, elemIndex, find, getAt)
import Msgs
import Questionnaires.Common.Models exposing (QuestionnaireMigration)
import Questionnaires.Migration.DiffOverview.DiffOverview as DiffOverview
import Questionnaires.Migration.DiffTree.DiffTree as DiffTree
import Questionnaires.Migration.Models exposing (ChapterDiffNodeData, DiffNode(..), DiffState(..), ExpansionState(..), KnowledgeModelDiffNodeData, Model, NodeUuids, QuestionDiffNodeData, TreeNode, TreeNodeType(..), getDiffStateUuid, stringifyPath)
import Questionnaires.Migration.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing
import SplitPane


fetchData : (Msg -> Msgs.Msg) -> AppState -> String -> Cmd Msgs.Msg
fetchData wrapMsg appState uuid =
    Cmd.map wrapMsg <|
        QuestionnairesApi.getQuestionnaireMigration uuid appState GetQuestionnaireMigrationCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetQuestionnaireMigrationCompleted result ->
            handleGetQuestionnaireMigrationCompleted model result

        SplitPaneMsg spMsg ->
            ( { model | splitPaneState = SplitPane.update spMsg model.splitPaneState }
            , Cmd.none
            )

        ToggleDiffTreeNode uuid ->
            handleToggleDiffTreeNode model uuid

        SelectDiffTreeNode uuid ->
            handleSelectDiffTreeNode model uuid

        ShowPreviousDiffEvent uuid ->
            handleShowPreviousDiffEvent model uuid

        ShowNextDiffEvent uuid ->
            handleNextDiffEvent model uuid

        SetMigrationChangeAsNeedsReview uuid path ->
            handleSetMigrationChangeAsNeedsReview wrapMsg appState model uuid path

        SetMigrationChangeAsResolved uuid path ->
            handleSetMigrationChangeAsResolved wrapMsg appState model uuid path

        SettingMigrationChangeCompleted result ->
            handleSettingMigrationChangeCompleted model result

        DeleteMigrationChange uuid path ->
            handleDeleteMigrationChange wrapMsg appState model uuid path

        DeleteMigrationChangeCompleted result ->
            handleDeleteMigrationChangeCompleted model result

        MigrateQuestionnaire uuid ->
            handleMigrateQuestionnaire wrapMsg appState model uuid

        MigrateQuestionnaireCompleted result ->
            handleMigrateQuestionnaireCompleted wrapMsg appState model


handleGetQuestionnaireMigrationCompleted : Model -> Result ApiError QuestionnaireMigration -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireMigrationCompleted model result =
    case result of
        Ok questionnaireMigration ->
            let
                previousKnowledgeModel =
                    questionnaireMigration.previousKnowledgeModel

                diffKnowledgeModel =
                    questionnaireMigration.diffKnowledgeModel

                diffEvents =
                    questionnaireMigration.diffEvents

                convertedEvents =
                    convertDiffEventToDiffState previousKnowledgeModel diffKnowledgeModel diffEvents

                diffStates =
                    appendUnchangedKnowledgeModelNodes diffKnowledgeModel convertedEvents

                diffTree =
                    createDiffTree diffKnowledgeModel questionnaireMigration.questionnaire.replies

                diffEventsUuids =
                    createDiffEventUuids convertedEvents diffTree diffKnowledgeModel.uuid

                activeNode =
                    List.head diffEventsUuids
                        |> Maybe.withDefault
                            { treeUuid = diffKnowledgeModel.uuid
                            , diffStateUuid = diffKnowledgeModel.uuid
                            }
            in
            ( { model
                | questionnaireMigration = Success questionnaireMigration
                , diffStates = Just <| diffStates
                , diffTree = Just diffTree
                , activeNode = activeNode
                , diffEventsUuids = diffEventsUuids
              }
            , Cmd.none
            )

        Err error ->
            ( { model | questionnaireMigration = getServerError error "Unable to load questionnaire migration." }
            , getResultCmd result
            )


handleMigrateQuestionnaire : (Msg -> Msgs.Msg) -> AppState -> Model -> String -> ( Model, Cmd Msgs.Msg )
handleMigrateQuestionnaire wrapMsg appState model uuid =
    let
        cmd =
            Cmd.map wrapMsg <|
                QuestionnairesApi.putMigrateQuestionnaire uuid appState MigrateQuestionnaireCompleted
    in
    ( model, cmd )


handleMigrateQuestionnaireCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleMigrateQuestionnaireCompleted wrapMsg appState model =
    ( model, Routing.cmdNavigate appState.key <| Routing.Questionnaires Index )


createDiffEventUuids : Dict.Dict String DiffState -> Dict.Dict String TreeNode -> String -> List NodeUuids
createDiffEventUuids diffStates diffTree treeUuid =
    case Dict.get treeUuid diffTree of
        Just node ->
            let
                children =
                    List.concatMap (createDiffEventUuids diffStates diffTree) node.children
            in
            case Dict.get node.uuid diffStates of
                Just state ->
                    let
                        uuids =
                            { treeUuid = treeUuid
                            , diffStateUuid = getDiffStateUuid state
                            }
                    in
                    uuids :: children

                Nothing ->
                    children

        Nothing ->
            []


handleShowPreviousDiffEvent : Model -> NodeUuids -> ( Model, Cmd Msgs.Msg )
handleShowPreviousDiffEvent model node =
    let
        maybePreviousEventUuid =
            elemIndex node model.diffEventsUuids
                |> Maybe.andThen (\index -> getAt (index - 1) model.diffEventsUuids)
    in
    case maybePreviousEventUuid of
        Nothing ->
            ( model, Cmd.none )

        Just previousEventUuid ->
            ( { model | activeNode = previousEventUuid }, Cmd.none )


handleNextDiffEvent : Model -> NodeUuids -> ( Model, Cmd Msgs.Msg )
handleNextDiffEvent model node =
    let
        maybeNextEventUuid =
            elemIndex node model.diffEventsUuids
                |> Maybe.andThen (\index -> getAt (index + 1) model.diffEventsUuids)
    in
    case maybeNextEventUuid of
        Nothing ->
            ( model, Cmd.none )

        Just nextEventUuid ->
            ( { model | activeNode = nextEventUuid }, Cmd.none )


handleToggleDiffTreeNode : Model -> String -> ( Model, Cmd Msgs.Msg )
handleToggleDiffTreeNode model uuid =
    let
        newModel =
            case model.diffTree of
                Just diffTree ->
                    { model | diffTree = Just <| toggleDiffTreeNodeExpansionStateByUuid diffTree uuid }

                Nothing ->
                    model
    in
    ( newModel, Cmd.none )


handleSelectDiffTreeNode : Model -> TreeNode -> ( Model, Cmd Msgs.Msg )
handleSelectDiffTreeNode model node =
    let
        activeNode =
            find (\n -> n.treeUuid == node.treeUuid) model.diffEventsUuids
                |> Maybe.withDefault
                    { treeUuid = node.treeUuid
                    , diffStateUuid = node.uuid
                    }
    in
    ( { model | activeNode = activeNode }, Cmd.none )


handleSetMigrationChangeAsNeedsReview : (Msg -> Msgs.Msg) -> AppState -> Model -> String -> List String -> ( Model, Cmd Msgs.Msg )
handleSetMigrationChangeAsNeedsReview wrapMsg appState model uuid questionPath =
    handleSetMigrationQuestionFlag wrapMsg appState model uuid questionPath NeedsReview


handleSetMigrationChangeAsResolved : (Msg -> Msgs.Msg) -> AppState -> Model -> String -> List String -> ( Model, Cmd Msgs.Msg )
handleSetMigrationChangeAsResolved wrapMsg appState model uuid questionPath =
    handleSetMigrationQuestionFlag wrapMsg appState model uuid questionPath MigrationResolved


handleSetMigrationQuestionFlag : (Msg -> Msgs.Msg) -> AppState -> Model -> String -> List String -> QuestionFlagType -> ( Model, Cmd Msgs.Msg )
handleSetMigrationQuestionFlag wrapMsg appState model uuid questionPath flagType =
    let
        flag =
            { questionPath = questionPath
            , flagTypes = [ flagType ]
            }

        body =
            encodeQuestionFlag flag

        newModel =
            { model | settingQuestionFlag = Loading, questionFlagToBeAdded = Just flag }

        cmd =
            Cmd.map wrapMsg <|
                QuestionnairesApi.putQuestionnaireMigrationQuestionFlag uuid body appState SettingMigrationChangeCompleted
    in
    ( newModel, cmd )


handleDeleteMigrationChange : (Msg -> Msgs.Msg) -> AppState -> Model -> String -> List String -> ( Model, Cmd Msgs.Msg )
handleDeleteMigrationChange wrapMsg appState model uuid path =
    let
        newModel =
            { model | settingQuestionFlag = Loading, questionFlagToBeRemovedPath = Just path }

        flag =
            { questionPath = path
            , flagTypes = []
            }

        body =
            encodeQuestionFlag flag

        cmd =
            Cmd.map wrapMsg <|
                QuestionnairesApi.putQuestionnaireMigrationQuestionFlag uuid body appState DeleteMigrationChangeCompleted
    in
    ( newModel, cmd )


handleDeleteMigrationChangeCompleted : Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handleDeleteMigrationChangeCompleted model result =
    case ( result, model.questionnaireMigration ) of
        ( Ok _, Success questionnaireMigration ) ->
            let
                path =
                    model.questionFlagToBeRemovedPath
                        |> Maybe.withDefault []

                oldFlags =
                    questionnaireMigration.questionnaire.questionFlags

                newFlags =
                    oldFlags
                        |> List.filter (\flag -> flag.questionPath /= path)

                oldQuestionnaire =
                    questionnaireMigration.questionnaire

                newQuestionnaire =
                    { oldQuestionnaire | questionFlags = newFlags }

                newQuestionnaireMigration =
                    { questionnaireMigration | questionnaire = newQuestionnaire }

                newModel =
                    { model
                        | questionnaireMigration = Success newQuestionnaireMigration
                        , questionFlagToBeRemovedPath = Nothing
                        , removingQuestionFlag = Success ""
                    }
            in
            ( newModel, Cmd.none )

        ( Err error, _ ) ->
            ( { model | removingQuestionFlag = getServerError error "Unable to remove question flag." }
            , getResultCmd result
            )

        ( _, _ ) ->
            ( model, Cmd.none )


handleSettingMigrationChangeCompleted : Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handleSettingMigrationChangeCompleted model result =
    case ( result, model.questionnaireMigration ) of
        ( Ok _, Success questionnaireMigration ) ->
            let
                oldQuestionFlags =
                    questionnaireMigration.questionnaire.questionFlags

                newQuestionFlags =
                    case model.questionFlagToBeAdded of
                        Just flag ->
                            let
                                filteredOldFlags =
                                    List.filter (\f -> f.questionPath /= flag.questionPath) oldQuestionFlags
                            in
                            filteredOldFlags ++ [ flag ]

                        Nothing ->
                            oldQuestionFlags

                oldQuestionnaire =
                    questionnaireMigration.questionnaire

                newQuestionnaire =
                    { oldQuestionnaire | questionFlags = newQuestionFlags }

                newMigration =
                    { questionnaireMigration | questionnaire = newQuestionnaire }
            in
            ( { model
                | settingQuestionFlag = Success ""
                , questionFlagToBeAdded = Nothing
                , questionnaireMigration = Success newMigration
              }
            , Cmd.none
            )

        ( Err error, _ ) ->
            ( { model | settingQuestionFlag = getServerError error "Unable to set question flag." }
            , getResultCmd result
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- ----------------
-- Diff Root Node
-- ----------------


createDiffTree : KnowledgeModel -> FormValues -> Dict.Dict String TreeNode
createDiffTree knowledgeModel replies =
    let
        children =
            List.map .uuid knowledgeModel.chapters

        path =
            []

        subtreeDict =
            List.foldl (createChapterDiffSubTree [] replies) Dict.empty knowledgeModel.chapters

        knowledgeModelNode =
            { uuid = knowledgeModel.uuid
            , treeUuid = knowledgeModel.uuid
            , children = children
            , title = knowledgeModel.name
            , expansionState = Expanded
            , path = path
            , nodeType = KnowledgeModelType
            }
    in
    Dict.insert knowledgeModel.uuid knowledgeModelNode subtreeDict


createChapterDiffSubTree : Path -> FormValues -> Chapter -> Dict.Dict String TreeNode -> Dict.Dict String TreeNode
createChapterDiffSubTree parentPath replies chapter diffTree =
    let
        children =
            List.map getQuestionUuid chapter.questions

        path =
            List.append parentPath [ ChapterPathNode chapter.uuid ]

        subtreeDict =
            List.foldl (createQuestionDiffSubTree path replies) diffTree chapter.questions

        chapterNode =
            { uuid = chapter.uuid
            , treeUuid = chapter.uuid
            , children = children
            , title = chapter.title
            , expansionState = Collapsed
            , path = path
            , nodeType = ChapterType
            }
    in
    Dict.insert chapter.uuid chapterNode subtreeDict


createQuestionDiffSubTree : Path -> FormValues -> Question -> Dict.Dict String TreeNode -> Dict.Dict String TreeNode
createQuestionDiffSubTree path replies question diffTree =
    createQuestionDiffSubTreeAtKey path (getQuestionUuid question) replies question diffTree


createQuestionDiffSubTreeAtKey : Path -> String -> FormValues -> Question -> Dict.Dict String TreeNode -> Dict.Dict String TreeNode
createQuestionDiffSubTreeAtKey parentPath insertionKey replies question diffTree =
    let
        uuid =
            getQuestionUuid question

        path =
            List.append parentPath [ QuestionPathNode insertionKey ]

        replyPath =
            stringifyPath path

        answersChildren =
            List.map .uuid <| getQuestionAnswers question

        reply =
            findQuestionReply replies replyPath

        questionsChildren : List ( Int, List Question )
        questionsChildren =
            if List.length (getQuestionItemQuestions question) == 0 then
                []

            else
                let
                    -- We always want to see at least one item (even though it's not answered)
                    subQuestionsCount =
                        case reply of
                            Just (ItemListReply count) ->
                                max 1 count

                            _ ->
                                1
                in
                List.range 0 (subQuestionsCount - 1)
                    |> List.map (\i -> ( i, getQuestionItemQuestions question ))

        withAnswersSubtreeDict =
            List.foldl (createAnswerDiffSubTree path replies) diffTree <| getQuestionAnswers question

        -- Create subtree for item templates
        subtreeWithSubquetsions =
            List.foldl
                (\( index, qs ) ( uuids, tree ) ->
                    let
                        st =
                            createListItemQuestionDiffSubTree path uuid index replies qs tree
                    in
                    ( uuids ++ [ Tuple.first st ], Tuple.second st )
                )
                ( [], withAnswersSubtreeDict )
                questionsChildren

        questionNode =
            { uuid = uuid
            , treeUuid = insertionKey
            , children = answersChildren ++ Tuple.first subtreeWithSubquetsions
            , title = getQuestionTitle question
            , expansionState = Collapsed
            , path = path
            , nodeType = QuestionType reply
            }
    in
    Dict.insert insertionKey questionNode (Tuple.second subtreeWithSubquetsions)


createListItemQuestionDiffSubTree : Path -> String -> Int -> FormValues -> List Question -> Dict.Dict String TreeNode -> ( String, Dict.Dict String TreeNode )
createListItemQuestionDiffSubTree parentPath parentQuestionUuid itemIndex replies questions diffTree =
    let
        uuid =
            [ parentQuestionUuid, "itemTeplate", String.fromInt itemIndex ]
                |> String.join "."

        questionInsertionKey q =
            [ String.fromInt itemIndex, getQuestionUuid q ]
                |> String.join "."

        children =
            List.map questionInsertionKey questions

        subtreeDict =
            List.foldl (\q tree -> createQuestionDiffSubTreeAtKey parentPath (questionInsertionKey q) replies q tree) diffTree questions

        itemNode =
            { uuid = uuid
            , treeUuid = uuid
            , children = children
            , title = "Item template"
            , expansionState = Collapsed
            , path = [] -- Itentional, we use item template as graphic element only
            , nodeType = QuestionListItemType
            }
    in
    ( uuid, Dict.insert uuid itemNode subtreeDict )


findQuestionReply : FormValues -> String -> Maybe ReplyValue
findQuestionReply replies path =
    find (\v -> v.path == path) replies
        |> Maybe.map .value


createAnswerDiffSubTree : Path -> FormValues -> Answer -> Dict.Dict String TreeNode -> Dict.Dict String TreeNode
createAnswerDiffSubTree parentPath replies answer diffTree =
    let
        path =
            List.append parentPath [ AnswerPathNode answer.uuid ]

        children =
            List.map getQuestionUuid (getFollowUpQuestions answer)

        subtreeDict =
            List.foldl (createQuestionDiffSubTree path replies) diffTree <| getFollowUpQuestions answer

        answerNode =
            { uuid = answer.uuid
            , treeUuid = answer.uuid
            , children = children
            , title = answer.label
            , expansionState = Collapsed
            , path = path
            , nodeType = AnswerType
            }
    in
    Dict.insert answer.uuid answerNode subtreeDict


getFollowUpQuestions : Answer -> List Question
getFollowUpQuestions answer =
    case answer.followUps of
        FollowUps questions ->
            questions


toggleDiffTreeNodeExpansionStateByUuid : Dict.Dict String TreeNode -> String -> Dict.Dict String TreeNode
toggleDiffTreeNodeExpansionStateByUuid diffTree uuid =
    case Dict.get uuid diffTree of
        Just node ->
            Dict.insert uuid { node | expansionState = toggleExpansionState node.expansionState } diffTree

        Nothing ->
            diffTree


toggleExpansionState : ExpansionState -> ExpansionState
toggleExpansionState state =
    case state of
        Expanded ->
            Collapsed

        Collapsed ->
            Expanded



-- ----------------
-- Diff State Model
-- ----------------


convertDiffEventToDiffState : KnowledgeModel -> KnowledgeModel -> List Event -> Dict String DiffState
convertDiffEventToDiffState previousKnowledgeModel diffKnowledgeModel =
    let
        insertDiffEvent event dict =
            case event of
                AddKnowledgeModelEvent data _ ->
                    Dict.insert data.kmUuid (createKnowledgeModelCreatedState diffKnowledgeModel) dict

                EditKnowledgeModelEvent data _ ->
                    if Dict.member data.kmUuid dict then
                        dict

                    else
                        Dict.insert data.kmUuid (createKnowledgeModelModifiedState previousKnowledgeModel diffKnowledgeModel) dict

                AddChapterEvent data _ ->
                    Dict.insert data.chapterUuid (createChapterCreatedState data.chapterUuid diffKnowledgeModel) dict

                EditChapterEvent data _ ->
                    if Dict.member data.chapterUuid dict then
                        dict

                    else
                        Dict.insert data.chapterUuid (createChapterModifiedState data.chapterUuid previousKnowledgeModel diffKnowledgeModel) dict

                DeleteChapterEvent data _ ->
                    Dict.insert data.chapterUuid (createChapterRemovedState data.chapterUuid diffKnowledgeModel) dict

                AddTagEvent _ _ ->
                    dict

                EditTagEvent _ _ ->
                    dict

                DeleteTagEvent _ _ ->
                    dict

                AddQuestionEvent data _ ->
                    let
                        uuid =
                            getAddQuestionUuid data
                    in
                    Dict.insert uuid (createQuestionCreatedState uuid diffKnowledgeModel) dict

                EditQuestionEvent data _ ->
                    let
                        uuid =
                            getEditQuestionUuid data
                    in
                    if Dict.member uuid dict then
                        dict

                    else
                        Dict.insert uuid (createQuestionModifiedState uuid previousKnowledgeModel diffKnowledgeModel) dict

                DeleteQuestionEvent data _ ->
                    Dict.insert data.questionUuid (createQuestionRemovedState data.questionUuid diffKnowledgeModel) dict

                AddAnswerEvent data _ ->
                    Dict.insert data.answerUuid (createAnswerCreatedState data.answerUuid diffKnowledgeModel) dict

                EditAnswerEvent data _ ->
                    if Dict.member data.answerUuid dict then
                        dict

                    else
                        Dict.insert data.answerUuid (createAnswerModifiedState data.answerUuid previousKnowledgeModel diffKnowledgeModel) dict

                DeleteAnswerEvent data _ ->
                    Dict.insert data.answerUuid (createAnswerDeletedState data.answerUuid diffKnowledgeModel) dict

                AddReferenceEvent _ _ ->
                    dict

                EditReferenceEvent _ _ ->
                    dict

                DeleteReferenceEvent _ _ ->
                    dict

                AddExpertEvent _ _ ->
                    dict

                EditExpertEvent _ _ ->
                    dict

                DeleteExpertEvent _ _ ->
                    dict

                AddIntegrationEvent _ _ ->
                    dict

                EditIntegrationEvent _ _ ->
                    dict

                DeleteIntegrationEvent _ _ ->
                    dict
    in
    List.foldl insertDiffEvent Dict.empty


appendUnchangedKnowledgeModelNodes : KnowledgeModel -> Dict String DiffState -> Dict String DiffState
appendUnchangedKnowledgeModelNodes knowledgeModel diffStates =
    let
        updatedDiffStates =
            List.foldl appendUnchagnedChapterNodes diffStates knowledgeModel.chapters
    in
    insertDiffStateIfNotExists updatedDiffStates <|
        createKnowledgeModelUnchangedState knowledgeModel


appendUnchagnedChapterNodes : Chapter -> Dict String DiffState -> Dict String DiffState
appendUnchagnedChapterNodes chapter diffStates =
    let
        updatedDiffStates =
            List.foldl appendUnchangedQuestionNodes diffStates chapter.questions
    in
    insertDiffStateIfNotExists updatedDiffStates <|
        createChapterUnchangedState chapter


appendUnchangedQuestionNodes : Question -> Dict String DiffState -> Dict String DiffState
appendUnchangedQuestionNodes question diffStates =
    let
        withAnswersDiffStates =
            List.foldl appendUnchangedAnswerNodes diffStates (getQuestionAnswers question)

        updatedDiffStates =
            List.foldl appendUnchangedQuestionNodes withAnswersDiffStates (getQuestionItemQuestions question)
    in
    insertDiffStateIfNotExists updatedDiffStates <|
        createQuestionUnchangedState question


appendUnchangedAnswerNodes : Answer -> Dict String DiffState -> Dict String DiffState
appendUnchangedAnswerNodes answer diffStates =
    let
        updatedDiffStates =
            List.foldl appendUnchangedQuestionNodes diffStates (getFollowUpQuestions answer)
    in
    insertDiffStateIfNotExists updatedDiffStates <|
        createAnswerUnchangedState answer


insertDiffStateIfNotExists : Dict String DiffState -> DiffState -> Dict String DiffState
insertDiffStateIfNotExists diffStates state =
    let
        uuid =
            getDiffStateUuid state
    in
    case Dict.get uuid diffStates of
        Just node ->
            diffStates

        Nothing ->
            Dict.insert uuid state diffStates


createKnowledgeModelCreatedState : KnowledgeModel -> DiffState
createKnowledgeModelCreatedState knowledgeModel =
    Created <|
        KnowledgeModelDiffNode <|
            { uuid = knowledgeModel.uuid
            , name = [ Diff.Added knowledgeModel.name ]
            }


createKnowledgeModelModifiedState : KnowledgeModel -> KnowledgeModel -> DiffState
createKnowledgeModelModifiedState prevKm diffKm =
    let
        nameDiff =
            diffStrings prevKm.name diffKm.name
    in
    Modified
        (KnowledgeModelDiffNode
            { uuid = prevKm.uuid
            , name = oldNodeDiffChanges nameDiff
            }
        )
        (KnowledgeModelDiffNode
            { uuid = diffKm.uuid
            , name = newNodeDiffChanges nameDiff
            }
        )


createKnowledgeModelUnchangedState : KnowledgeModel -> DiffState
createKnowledgeModelUnchangedState knowledgeModel =
    Unchanged <|
        KnowledgeModelDiffNode <|
            { uuid = knowledgeModel.uuid
            , name = [ Diff.NoChange knowledgeModel.name ]
            }


createChapterCreatedState : String -> KnowledgeModel -> DiffState
createChapterCreatedState uuid knowledgeModel =
    let
        chapter =
            findChapterByUuid uuid knowledgeModel
    in
    Created <|
        ChapterDiffNode <|
            { uuid = chapter.uuid
            , title = [ Diff.Added chapter.title ]
            , text = [ Diff.Added chapter.text ]
            }


createChapterModifiedState : String -> KnowledgeModel -> KnowledgeModel -> DiffState
createChapterModifiedState uuid previousKnowledgeModel newKnowledgeModel =
    let
        oldChapter =
            findChapterByUuid uuid previousKnowledgeModel

        newChapter =
            findChapterByUuid uuid newKnowledgeModel

        titleDiff =
            diffStrings oldChapter.title newChapter.title

        textDiff =
            diffStrings oldChapter.text newChapter.text
    in
    Modified
        (ChapterDiffNode
            { uuid = uuid
            , title = oldNodeDiffChanges titleDiff
            , text = oldNodeDiffChanges textDiff
            }
        )
        (ChapterDiffNode
            { uuid = uuid
            , title = newNodeDiffChanges titleDiff
            , text = newNodeDiffChanges textDiff
            }
        )


createChapterUnchangedState : Chapter -> DiffState
createChapterUnchangedState chapter =
    Unchanged <|
        ChapterDiffNode <|
            { uuid = chapter.uuid
            , title = [ Diff.NoChange chapter.title ]
            , text = [ Diff.NoChange chapter.text ]
            }


createChapterRemovedState : String -> KnowledgeModel -> DiffState
createChapterRemovedState uuid knowledgeModel =
    let
        chapter =
            findChapterByUuid uuid knowledgeModel
    in
    Removed <|
        ChapterDiffNode <|
            { uuid = uuid
            , title = [ Diff.Removed chapter.title ]
            , text = [ Diff.Removed chapter.text ]
            }


createQuestionCreatedState : String -> KnowledgeModel -> DiffState
createQuestionCreatedState uuid knowledgeModel =
    let
        question =
            findQuestionByUuid uuid knowledgeModel

        textDiff =
            getQuestionText question
                |> Maybe.map (List.singleton << Diff.Added)
                |> Maybe.withDefault []
    in
    Created <|
        QuestionDiffNode <|
            { uuid = uuid
            , title = [ Diff.Added <| getQuestionTitle question ]
            , text = textDiff
            , question = question
            }


createQuestionModifiedState : String -> KnowledgeModel -> KnowledgeModel -> DiffState
createQuestionModifiedState uuid previousKnowledgeModel diffKnowledgeModel =
    let
        oldQuestion =
            findQuestionByUuid uuid previousKnowledgeModel

        newQuestion =
            findQuestionByUuid uuid diffKnowledgeModel

        titleDiff =
            diffStrings (getQuestionTitle oldQuestion) (getQuestionTitle newQuestion)

        textDiff =
            Maybe.map2 diffStrings (getQuestionText oldQuestion) (getQuestionText newQuestion)
                |> Maybe.withDefault []
    in
    Modified
        (QuestionDiffNode
            { uuid = uuid
            , title = oldNodeDiffChanges titleDiff
            , text = oldNodeDiffChanges textDiff
            , question = oldQuestion
            }
        )
        (QuestionDiffNode
            { uuid = uuid
            , title = newNodeDiffChanges titleDiff
            , text = newNodeDiffChanges textDiff
            , question = newQuestion
            }
        )


createQuestionRemovedState : String -> KnowledgeModel -> DiffState
createQuestionRemovedState uuid knowledgeModel =
    let
        question =
            findQuestionByUuid uuid knowledgeModel

        textDiff =
            getQuestionText question
                |> Maybe.map (List.singleton << Diff.Removed)
                |> Maybe.withDefault []
    in
    Removed <|
        QuestionDiffNode <|
            { uuid = uuid
            , title = [ Diff.Removed <| getQuestionTitle question ]
            , text = textDiff
            , question = question
            }


createQuestionUnchangedState : Question -> DiffState
createQuestionUnchangedState question =
    let
        text =
            getQuestionText question
                |> Maybe.map (List.singleton << Diff.NoChange)
                |> Maybe.withDefault []
    in
    Unchanged <|
        QuestionDiffNode <|
            { uuid = getQuestionUuid question
            , title = [ Diff.NoChange <| getQuestionTitle question ]
            , text = text
            , question = question
            }


createAnswerCreatedState : String -> KnowledgeModel -> DiffState
createAnswerCreatedState uuid knowledgeModel =
    let
        answer =
            findAnswerByUuid uuid knowledgeModel

        adviceDiff =
            answer.advice
                |> Maybe.map (List.singleton << Diff.Added)
                |> Maybe.withDefault []
    in
    Created <|
        AnswerDiffNode <|
            { uuid = answer.uuid
            , label = [ Diff.Added answer.label ]
            , advice = adviceDiff
            , answer = answer
            }


createAnswerModifiedState : String -> KnowledgeModel -> KnowledgeModel -> DiffState
createAnswerModifiedState uuid previousKnowledgeModel diffKnowledgeModel =
    let
        oldAnswer =
            findAnswerByUuid uuid previousKnowledgeModel

        newAnswer =
            findAnswerByUuid uuid diffKnowledgeModel

        labelDiff =
            diffStrings oldAnswer.label newAnswer.label

        adviceDiff =
            Maybe.map2 diffStrings oldAnswer.advice newAnswer.advice
                |> Maybe.withDefault []
    in
    Modified
        (AnswerDiffNode
            { uuid = uuid
            , label = oldNodeDiffChanges labelDiff
            , advice = oldNodeDiffChanges adviceDiff
            , answer = oldAnswer
            }
        )
        (AnswerDiffNode
            { uuid = uuid
            , label = newNodeDiffChanges labelDiff
            , advice = newNodeDiffChanges adviceDiff
            , answer = newAnswer
            }
        )


createAnswerDeletedState : String -> KnowledgeModel -> DiffState
createAnswerDeletedState uuid knowledgeModel =
    let
        answer =
            findAnswerByUuid uuid knowledgeModel

        adviceDiff =
            answer.advice
                |> Maybe.map (List.singleton << Diff.Added)
                |> Maybe.withDefault []
    in
    Removed <|
        AnswerDiffNode <|
            { uuid = uuid
            , label = [ Diff.Removed answer.label ]
            , advice = adviceDiff
            , answer = answer
            }


createAnswerUnchangedState : Answer -> DiffState
createAnswerUnchangedState answer =
    let
        advice =
            answer.advice
                |> Maybe.map (List.singleton << Diff.NoChange)
                |> Maybe.withDefault []
    in
    Unchanged <|
        AnswerDiffNode <|
            { uuid = answer.uuid
            , label = [ Diff.NoChange answer.label ]
            , advice = advice
            , answer = answer
            }


dummyChapter : Chapter
dummyChapter =
    { uuid = ""
    , title = ""
    , text = ""
    , questions = []
    }


dummyQuestion : Question
dummyQuestion =
    OptionsQuestion <|
        { uuid = ""
        , title = ""
        , text = Nothing
        , requiredLevel = Nothing
        , tagUuids = []
        , references = []
        , experts = []
        , answers = []
        }


dummyAnswer : Answer
dummyAnswer =
    { uuid = ""
    , label = ""
    , advice = Nothing
    , metricMeasures = []
    , followUps = FollowUps []
    }


findChapterByUuid : String -> KnowledgeModel -> Chapter
findChapterByUuid uuid knowledgeModel =
    find (\chapter -> chapter.uuid == uuid) knowledgeModel.chapters
        |> Maybe.withDefault dummyChapter


findQuestionByUuid : String -> KnowledgeModel -> Question
findQuestionByUuid uuid knowledgeModel =
    getKnowledgeModelQuestions knowledgeModel
        |> find (\question -> getQuestionUuid question == uuid)
        |> Maybe.withDefault dummyQuestion


findAnswerByUuid : String -> KnowledgeModel -> Answer
findAnswerByUuid uuid knowledgeModel =
    getKnowledgeModelAnswers knowledgeModel
        |> find (\answer -> answer.uuid == uuid)
        |> Maybe.withDefault dummyAnswer


diffStrings : String -> String -> List (Diff.Change String)
diffStrings l r =
    let
        stringChars =
            String.toList
    in
    Diff.diff (stringChars l) (stringChars r)
        |> List.map (mapDiffChange String.fromChar)


mapDiffChange : (a -> b) -> Diff.Change a -> Diff.Change b
mapDiffChange f change =
    case change of
        Diff.Added c ->
            Diff.Added <| f c

        Diff.Removed c ->
            Diff.Removed <| f c

        Diff.NoChange c ->
            Diff.NoChange <| f c


getKnowledgeModelQuestions : KnowledgeModel -> List Question
getKnowledgeModelQuestions knowledgeModel =
    knowledgeModel.chapters
        |> andThen getChapterQuestions


getChapterQuestions : Chapter -> List Question
getChapterQuestions chapter =
    chapter.questions
        |> andThen unwrapWithNestedQuestions


getKnowledgeModelAnswers : KnowledgeModel -> List Answer
getKnowledgeModelAnswers knowledgeModel =
    getKnowledgeModelQuestions knowledgeModel
        |> andThen getQuestionAnswers


unwrapWithNestedQuestions : Question -> List Question
unwrapWithNestedQuestions question =
    let
        nestedQuestions =
            getQuestionItemQuestions question
                |> andThen unwrapWithNestedQuestions
    in
    question :: nestedQuestions


oldNodeDiffChanges : List (Diff.Change a) -> List (Diff.Change a)
oldNodeDiffChanges =
    let
        oldChanges event =
            case event of
                Diff.Added _ ->
                    False

                _ ->
                    True
    in
    List.filter oldChanges


newNodeDiffChanges : List (Diff.Change a) -> List (Diff.Change a)
newNodeDiffChanges =
    let
        newChanges event =
            case event of
                Diff.Removed _ ->
                    False

                _ ->
                    True
    in
    List.filter newChanges
