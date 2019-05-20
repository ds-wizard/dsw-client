module Questionnaires.Migration.View exposing (view)

import Auth.Models exposing (JwtToken)
import Common.Html exposing (emptyNode, fa)
import Common.View.Page as Page
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs
import Questionnaires.Common.Models exposing (QuestionnaireMigration)
import Questionnaires.Migration.DiffOverview.DiffOverview as DiffOverview
import Questionnaires.Migration.DiffTree.DiffTree as DiffTree
import Questionnaires.Migration.Models exposing (DiffNode(..), Model, NodeUuids, hasFollowingDiffEvent, hasPreviousDiffEvent)
import Questionnaires.Migration.Msgs exposing (Msg(..))
import SplitPane exposing (ViewConfig, createViewConfig)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    Page.actionResultView (contentView wrapMsg model) model.questionnaireMigration
        |> Html.map wrapMsg


finishMigrationAction : String -> Html Msg
finishMigrationAction uuid =
    button
        [ class "btn btn-primary ml-auto"
        , onClick <| MigrateQuestionnaire uuid
        ]
        [ text "Finalize migration"
        ]


contentView : (Msg -> Msgs.Msg) -> Model -> QuestionnaireMigration -> Html Msg
contentView wrapMsg model questionnaireMigration =
    case ( model.diffStates, model.diffTree ) of
        ( Just diffStates, Just diffTree ) ->
            case ( Dict.get model.activeNode.diffStateUuid diffStates, Dict.get model.activeNode.treeUuid diffTree ) of
                ( Just diffState, Just treeNode ) ->
                    let
                        node =
                            model.activeNode

                        diffNavigation =
                            if List.member node model.diffEventsUuids then
                                diffNavigationView model node

                            else
                                emptyNode

                        questionnaireUuid =
                            questionnaireMigration.questionnaire.uuid

                        questionFlags =
                            questionnaireMigration.questionnaire.questionFlags

                        replies =
                            questionnaireMigration.questionnaire.replies

                        leftPane =
                            DiffTree.view node.treeUuid questionnaireMigration.diffKnowledgeModel.uuid diffTree diffStates

                        rightPane =
                            div [ class "col", id "diff-overview-view" ]
                                [ diffNavigation
                                , DiffOverview.view questionnaireUuid diffState treeNode replies diffTree questionFlags
                                ]
                    in
                    div [ class "Questionnaire__Migration" ]
                        [ div [ class "questionnaire-migration-header" ]
                            [ h5 [] [ text "Questionnaire migration" ]
                            , finishMigrationAction questionnaireMigration.questionnaire.uuid
                            ]
                        , SplitPane.view splitPaneConfig leftPane rightPane model.splitPaneState
                        ]

                _ ->
                    emptyNode

        _ ->
            emptyNode


diffNavigationView : Model -> NodeUuids -> Html Msg
diffNavigationView model node =
    div [ class "row mt-4 px-3" ]
        [ div [ class "col" ]
            [ button
                [ class "btn btn-outline-primary"
                , onClick (ShowPreviousDiffEvent node)
                , hidden <| not (hasPreviousDiffEvent model node)
                ]
                [ text "Previous" ]
            ]
        , div [ class "col text-right" ]
            [ button
                [ class "btn btn-outline-primary"
                , onClick (ShowNextDiffEvent node)
                , hidden <| not (hasFollowingDiffEvent model node)
                ]
                [ text "Next" ]
            ]
        ]


splitPaneConfig : ViewConfig Msg
splitPaneConfig =
    createViewConfig
        { toMsg = SplitPaneMsg
        , customSplitter = Nothing
        }
