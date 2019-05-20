module Questionnaires.Migration.DiffTree.DiffTree exposing (view)

import Common.Html exposing (emptyNode, fa)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (Answer, Chapter, FollowUps(..), KnowledgeModel, Question, getQuestionAnswers, getQuestionTitle, getQuestionUuid)
import KMEditor.Common.Models.Events exposing (Event(..), getEventEntityUuid)
import Msgs
import Questionnaires.Common.Models exposing (QuestionnaireMigration)
import Questionnaires.Migration.Models exposing (DiffState(..), ExpansionState(..), Model, TreeNode, TreeNodeType(..))
import Questionnaires.Migration.Msgs exposing (Msg(..))


view : String -> String -> Dict.Dict String TreeNode -> Dict.Dict String DiffState -> Html Msg
view activeUuid rootUuid nodes diffStates =
    let
        tree =
            treeNodeView activeUuid nodes diffStates rootUuid
                |> Tuple.mapSecond List.singleton
                |> Tuple.mapSecond (ul [])
                |> Tuple.second
    in
    div [ class "tree-col" ]
        [ div [ class "diff-tree" ] [ tree ]
        ]


treeNodeView : String -> Dict.Dict String TreeNode -> Dict.Dict String DiffState -> String -> ( Bool, Html Msg )
treeNodeView activeUuid nodes diffStates rootUuid =
    case Dict.get rootUuid nodes of
        Just node ->
            let
                ( hasActiveChild, children ) =
                    List.map (treeNodeView activeUuid nodes diffStates) node.children
                        |> foldTreeChildNodesWithActiveIndication
                        |> Tuple.mapSecond (ul [])

                stateCaret =
                    if not <| List.isEmpty node.children then
                        caret (ToggleDiffTreeNode rootUuid) node.expansionState hasActiveChild

                    else
                        emptyNode

                nodeContent =
                    [ fa <| nodeTypeIcon node.nodeType
                    , span [] [ text node.title ]
                    ]

                diffClass =
                    Dict.get node.uuid diffStates
                        |> Maybe.map diffStateClassAttribute
                        |> Maybe.withDefault (class "state-unchanged")

                action =
                    case node.nodeType of
                        QuestionListItemType ->
                            span [] nodeContent

                        _ ->
                            a [ onClick <| SelectDiffTreeNode node ] nodeContent

                treeNode =
                    li
                        [ diffClass
                        , classList [ ( "active", rootUuid == activeUuid ) ]
                        ]
                        [ stateCaret
                        , action
                        , if node.expansionState == Expanded then
                            children

                          else
                            emptyNode
                        ]
            in
            ( hasActiveChild || rootUuid == activeUuid, treeNode )

        _ ->
            ( False, emptyNode )


foldTreeChildNodesWithActiveIndication : List ( Bool, Html msg ) -> ( Bool, List (Html msg) )
foldTreeChildNodesWithActiveIndication items =
    let
        hasActiveChild =
            List.foldl ((||) << Tuple.first) False items

        children =
            List.map Tuple.second items
    in
    ( hasActiveChild, children )


nodeTypeIcon : TreeNodeType -> String
nodeTypeIcon nodeType =
    case nodeType of
        KnowledgeModelType ->
            "database"

        ChapterType ->
            "book"

        QuestionType _ ->
            "comment-o"

        AnswerType ->
            "check-square-o"

        QuestionListItemType ->
            "file-text-o"


diffStateClassAttribute : DiffState -> Html.Attribute msg
diffStateClassAttribute state =
    let
        stateClass =
            case state of
                Unchanged _ ->
                    "state-unchanged"

                Modified _ _ ->
                    "state-edited"

                Removed _ ->
                    "state-removed"

                Created _ ->
                    "state-added"
    in
    class stateClass


caret : Msg -> ExpansionState -> Bool -> Html Msg
caret msg state hasActiveChild =
    let
        open =
            state == Expanded
    in
    a [ onClick msg, class "caret" ]
        [ i
            [ class "fa"
            , classList
                [ ( "fa-caret-right", not open )
                , ( "fa-caret-down", open )
                , ( "caret-active", hasActiveChild )
                ]
            ]
            []
        ]
