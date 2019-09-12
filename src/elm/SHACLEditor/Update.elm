module SHACLEditor.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult)
import Common.Api.Levels as LevelsApi
import Common.Api.SHACLs as SHACLsApi
import Common.AppState exposing (AppState)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import Common.Setters exposing (setLevels)
import Msgs
import SHACLEditor.Models exposing (CurrentEditor(..), Model, createQuestionnaireModel)
import SHACLEditor.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    LevelsApi.getLevels appState GetLevelsCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        OpenSHACLEditor ->
            ( { model | currentEditor = SHACLEditor }, Cmd.none )

        OpenPreviewEditor ->
            ( { model
                | currentEditor = PreviewEditor
                , questionnaire = Loading
              }
            , Cmd.map wrapMsg <| SHACLsApi.fetchPreview model.shacl appState GetPreviewCompleted
            )

        ChangeSHACL newShacl ->
            ( { model | shacl = newShacl }, Cmd.none )

        GetLevelsCompleted result ->
            applyResult
                { setResult = setLevels
                , defaultError = "Unable to get levels"
                , model = model
                , result = result
                }

        GetPreviewCompleted result ->
            applyResult
                { setResult = createQuestionnaireModel appState
                , defaultError = "Unable to get preview"
                , model = model
                , result = result
                }

        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg wrapMsg appState model


handleQuestionnaireMsg : Common.Questionnaire.Msgs.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireMsg msg wrapMsg appState model =
    let
        ( questionnaire, cmd ) =
            case model.questionnaire of
                Success q ->
                    let
                        ( newQuestionnaire, newCmd ) =
                            Common.Questionnaire.Update.update msg appState q
                    in
                    ( Success newQuestionnaire, newCmd )

                _ ->
                    ( model.questionnaire, Cmd.none )
    in
    ( { model | questionnaire = questionnaire }
    , Cmd.map (wrapMsg << QuestionnaireMsg) cmd
    )
