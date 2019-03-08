module Questionnaires.Migration.DiffOverview.DiffOverview exposing ( view )

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Common.Questionnaire.Models exposing (QuestionFlags, QuestionFlagType(..), findQuestionFlagAtPath, getReply)
import Dict
import Diff
import FormEngine.Model exposing (FormValues, ReplyValue(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Questionnaires.Common.Models exposing (QuestionnaireMigration)
import Questionnaires.Migration.Models exposing (Model, TreeNode, DiffState (..), DiffNode(..), KnowledgeModelDiffNodeData, ChapterDiffNodeData, QuestionDiffNodeData, AnswerDiffNodeData, NodeUuids, stateNodes, getParentQuestionPathForUuid, isUnchangedState, stringifyPath)
import Questionnaires.Migration.Msgs exposing (Msg(..))
import KMEditor.Common.Models.Entities exposing (Question(..), Answer, getQuestionAnswers)


view : String -> DiffState -> TreeNode -> FormValues -> Dict.Dict String TreeNode -> List QuestionFlags -> Html Msg
view questionnaireUuid state treeNode replies diffTree flags =
    let
        ( oldNode, newNode ) =
            stateNodes state

        questionPath =
            getParentQuestionPathForUuid treeNode.treeUuid diffTree

        isUnchanged =
            isUnchangedState state

        flagTypes =
            questionPath
                |> Maybe.andThen (findQuestionFlagAtPath flags)
                |> Maybe.map .flagTypes
                |> Maybe.withDefault []

        stateControls =
            case (questionPath, isUnchanged) of
                (Just path, False) ->
                    if List.isEmpty flagTypes then
                        diffStateControls questionnaireUuid path
                    else
                        undoStateControl flagTypes (DeleteMigrationChange questionnaireUuid path)
                _ ->
                    emptyNode
    in
    div [ class "col DiffOverview mt-4" ]
    [ div [ class "row mb-5"]
        [ diffOverviewView "Current version" treeNode oldNode replies diffTree
        , diffOverviewView "New version" treeNode newNode replies diffTree
        ]
    , stateControls
    ]


-- Navigation


stateControlsContaier : List (Html msg) -> Html msg
stateControlsContaier content =
    div [ class "row" ]
        [ div [ class "col-sm-6 ml-auto" ]
            [ div [ class "card" ]
                [ div [ class "card-body" ] content
                ]
            ]
        ]

diffStateControls : String -> List String -> Html Msg
diffStateControls questionnaireUuid questionPath =
    let
        needsReviewMsg =
            SetMigrationChangeAsNeedsReview questionnaireUuid questionPath

        resolvedMsg =
            SetMigrationChangeAsResolved questionnaireUuid questionPath
    in
    stateControlsContaier
        <| [ p [] [ text "There appears to be changes which might affect your answer. Do you want to review the question later?" ]
           , button [ class "btn ml-auto mr-2", class "btn-outline-secondary", onClick needsReviewMsg ] [ text "Review later" ]
           , button [ class "btn btn-outline-success", class "btn-outline-secondary", onClick resolvedMsg ] [ text "It's OK" ]
           ]
    

undoStateControl : List QuestionFlagType -> Msg -> Html Msg
undoStateControl flagTypes clickAction =
    let
        isNeedsReview =
            List.member NeedsReview flagTypes

        intro =
            if isNeedsReview then
                "This item is marked for later review."
            else
                "This item was already reviewed and you will not be reminded of it after the migration is completed."
    in
     stateControlsContaier
        <| [ p []
                [ text intro
                , text " Changed yor mind? You can "
                , button [ class "p-0 btn btn-link", onClick clickAction ] [ text "undo this action" ]
                , text "."
                ]
           ]


-- Diff tree


diffOverviewView : String -> TreeNode -> Maybe DiffNode -> FormValues -> Dict.Dict String TreeNode -> Html Msg
diffOverviewView title treeNode diffNode replies diffTree =
    let
        content =
            case diffNode of
                Nothing ->
                    p [] [ text "No content" ]

                Just node ->
                    diffOverviewContent node treeNode replies diffTree
    in
    div [ class "col-md-6 diff-overview" ]
        [ h3 [] [ text title ]
        , div [ class "card" ]
            [ div [ class "card-body"]
                [ content
                ]
            ]
        ]


diffOverviewContent : DiffNode -> TreeNode -> FormValues -> Dict.Dict String TreeNode -> Html msg
diffOverviewContent diffNode treeNode replies diffTree =
    case diffNode of
        KnowledgeModelDiffNode data ->
            knowledgeModelDiffOverview data

        ChapterDiffNode data ->
            chapterDiffOverview data

        QuestionDiffNode data ->
            questionDiffOverview data treeNode replies diffTree

        AnswerDiffNode data ->
            answerDiffOverview data


knowledgeModelDiffOverview : KnowledgeModelDiffNodeData -> Html msg
knowledgeModelDiffOverview data =
    div [ class "knowledgemodel-diff" ]
        [ h5 [] [ text "KnowledgeModel name"]
        , render data.name
        ]


chapterDiffOverview : ChapterDiffNodeData -> Html msg
chapterDiffOverview data =
    div [ class "chapter-diff" ]
        [ h5 [] [ text "Chapter title" ]
        , render data.title
        , h5 [] [ text "Chapter text" ]
        , render data.text
        ]


questionDiffOverview : QuestionDiffNodeData -> TreeNode -> FormValues -> Dict.Dict String TreeNode -> Html msg
questionDiffOverview data node replies diffTree =
    let
        path =
            stringifyPath node.path

        reply =
            getReply replies path

        answer =
            case data.question of
                OptionsQuestion _ ->
                    questionAnswerReplyPreview data.question reply

                ListQuestion _ ->
                    questionItemListReplyPreview reply

                ValueQuestion _ ->
                    questionStringReplyPreview reply

                IntegrationQuestion _ ->
                    Debug.todo ""
    in
    div [ class "question-diff" ]
        [ h5 [] [ text "Question title" ]
        , render data.title
        , h5 [] [ text "Question text" ]
        , render data.text
        , hr [] []
        , answer
        ]


answerDiffOverview : AnswerDiffNodeData -> Html msg
answerDiffOverview data =
    div [ class "answer-diff" ]
        [ h5 [] [ text "Answer title"]
        , render data.label
        , h5 [] [ text "Answer text" ]
        , render data.advice
        ]


questionStringReplyPreview : Maybe ReplyValue -> Html msg
questionStringReplyPreview reply =
    let
        content =
            case reply of
                Just (StringReply string) ->
                    input [ disabled True, value string] []

                Nothing ->
                    questionNoReplyPreview

                _ ->
                    cannotMigrateQuestionReplyText
    in
    div [] [ content ]


questionItemListReplyPreview : Maybe ReplyValue -> Html msg
questionItemListReplyPreview reply =
    let
        questionHint count =
            case count of
                0 ->
                    "This question has not been answered yet."

                1 ->
                    "This question has one item answer. You can preview it in left panel."

                _ ->
                    "This question has multiple item answers. You can preview it in left panel."

        content =
            case reply of
                Just (ItemListReply count) ->
                    text <| questionHint count

                Nothing ->
                    questionNoReplyPreview

                _ ->
                    cannotMigrateQuestionReplyText
    in
    div [] [ content ]


questionAnswerReplyPreview : Question -> Maybe ReplyValue -> Html msg
questionAnswerReplyPreview question reply =
    case reply of
        Just (AnswerReply uuid) ->
            getQuestionAnswers question
                |> List.map (questionAnswerReplyOptionPreview uuid)
                |> div []

        Nothing ->
            questionNoReplyPreview

        _ ->
            cannotMigrateQuestionReplyText

questionAnswerReplyOptionPreview : String -> Answer -> Html msg
questionAnswerReplyOptionPreview selectedUuid answer =
    let
        isSelected =
            answer.uuid == selectedUuid
    in
    div [ class "card" ]
        [ div [ class "card-body p-2" ]
            [ div [ class "form-group form-check" ]
                [ input [ type_ "checkbox", class "form-check-input", disabled True, checked isSelected ] []
                , label [ class "form-check-label" ] [ text answer.label ]
                ]
            ]
        ]

questionNoReplyPreview : Html msg
questionNoReplyPreview =
    div []
        [ p [] [ text "This question has not been answered yet." ]
        ]


cannotMigrateQuestionReplyText : Html msg
cannotMigrateQuestionReplyText =
    text "Reply for this question can not be migrated. Mark question as 'Needs review' if you want to be notified after migration."


-- Diff views

render : List (Diff.Change String) -> Html msg
render =
    p [] << List.map renderChange


renderChange : Diff.Change String -> Html msg
renderChange change =
    case change of
        Diff.Added s -> span [ class "state-added" ] [ text s ]
        Diff.Removed s -> span [ class "state-removed" ] [ text s ]
        Diff.NoChange s -> span [ class "state-unchanged" ] [ text s ]


knowledgeModelDiffView : String -> Html msg
knowledgeModelDiffView name =
    div []
        [ h5 [] [ text "KnowledgeModel name" ]
        , p [] [ text name ]
        ]


changeRemoved : Diff.Change a -> Bool
changeRemoved change =
    case change of
        Diff.Removed _ -> True
        _         -> False


changeAdded : Diff.Change a -> Bool
changeAdded change =
    case change of
        Diff.Added _ -> True
        _       -> False
