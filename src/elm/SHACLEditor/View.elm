module SHACLEditor.View exposing (view)

import ActionResult
import Common.AppState exposing (AppState)
import Common.Html exposing (fa)
import Common.Questionnaire.DefaultQuestionnaireRenderer exposing (defaultQuestionnaireRenderer)
import Common.Questionnaire.Models
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import SHACLEditor.Models exposing (CurrentEditor(..), Model)
import SHACLEditor.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.currentEditor of
                SHACLEditor ->
                    viewSHACLEditor model

                PreviewEditor ->
                    viewPreview appState model
    in
    div [ class "SHACLEditor" ]
        [ editorHeader appState model
        , div [ class "editor-body" ] [ content ]
        ]


editorHeader : AppState -> Model -> Html Msg
editorHeader appState model =
    div [ class "editor-header" ]
        [ div [ class "navigation" ]
            [ ul [ class "nav" ]
                [ a
                    [ class "nav-link"
                    , classList [ ( "active", model.currentEditor == SHACLEditor ) ]
                    , onClick OpenSHACLEditor
                    ]
                    [ fa "code", text "SHACL" ]
                , a
                    [ class "nav-link"
                    , classList [ ( "active", model.currentEditor == PreviewEditor ) ]
                    , onClick OpenPreviewEditor
                    ]
                    [ fa "eye", text "Preview" ]
                ]
            , div [ class "actions" ] []
            ]
        ]


viewSHACLEditor model =
    textarea [ class "editor-textarea", value model.shacl, onInput ChangeSHACL, placeholder "Insert SHACL here..." ] []


viewPreview appState model =
    Page.actionResultView appState (questionnaireView appState) <|
        ActionResult.combine model.questionnaire model.levels


questionnaireView : AppState -> ( Common.Questionnaire.Models.Model, List Level ) -> Html Msg
questionnaireView appState ( questionnaireModel, levels ) =
    let
        questionnaire =
            viewQuestionnaire
                { features = []
                , levels =
                    if appState.config.levelsEnabled then
                        Just levels

                    else
                        Nothing
                , getExtraQuestionClass = always Nothing
                , forceDisabled = False
                , createRenderer = defaultQuestionnaireRenderer appState
                }
                appState
                questionnaireModel
                |> Html.map QuestionnaireMsg
    in
    div [ class "col KMEditor__Editor__Preview" ]
        [ questionnaire
        ]
