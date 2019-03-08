module Questionnaires.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Packages as PackagesApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form
import KnowledgeModels.Common.Models exposing (PackageDetail)
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.ExportModal.Models exposing (setQuestionnaire)
import Questionnaires.Index.ExportModal.Update as ExportModal
import Questionnaires.Index.Models exposing (Model, QuestionnaireRow, initQuestionnaireRow, questionnaireUpgradeFormValidation, encodeQuestionnaireUpgradeForm)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (cmdNavigate)
import Utils exposing (versionIsGreater)

fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        QuestionnairesApi.getQuestionnaires appState GetQuestionnairesCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetQuestionnairesCompleted result ->
            getQuestionnairesCompleted model result

        ShowHideDeleteQuestionnaire questionnaire ->
            ( { model | questionnaireToBeDeleted = questionnaire, deletingQuestionnaire = Unset }, Cmd.none )

        ShowHideQuestionnaireUpgradeForm questionnaire ->
            handleShowHideQuestionnaireUpgradeForm wrapMsg appState questionnaire model

        UpgradeQuestionnaireForm formMsg ->
            handleQuestionnaireUpgradeForm wrapMsg appState formMsg model

        DeleteQuestionnaire ->
            handleDeleteQuestionnaire wrapMsg appState model

        DeleteQuestionnaireCompleted result ->
            deleteQuestionnaireCompleted wrapMsg appState model result

        ShowExportQuestionnaire questionnaire ->
            ( { model | exportModalModel = setQuestionnaire questionnaire model.exportModalModel }
            , Cmd.map (wrapMsg << ExportModalMsg) <| ExportModal.fetchData appState
            )

        ExportModalMsg exportModalMsg ->
            let
                ( exportModalModel, cmd ) =
                    ExportModal.update exportModalMsg (wrapMsg << ExportModalMsg) appState model.exportModalModel
            in
            ( { model | exportModalModel = exportModalModel }, cmd )

        DeleteQuestionnaireMigration uuid ->
            handleDeleteMigration wrapMsg appState uuid model

        DeleteQuestionnaireMigrationCompleted result ->
            deleteMigrationCompleted wrapMsg appState model result

        GetUpgradableKnowledgeModelsCompleted result ->
            handleUpgradableKnowledgeModelsCompleted wrapMsg result model

        PostQuestionnaireMigrationCompleted result ->
            handlePostQuestionnaireMigrationCompleted wrapMsg appState result model


getQuestionnairesCompleted : Model -> Result ApiError (List Questionnaire) -> ( Model, Cmd Msgs.Msg )
getQuestionnairesCompleted model result =
    case result of
        Ok questionnaires ->
            ( { model | questionnaires = Success questionnaires }
            , Cmd.none
            )

        Err error ->
            ( { model | questionnaires = getServerError error "Unable to get questionnaires." }
            , getResultCmd result
            )


handleDeleteQuestionnaire : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteQuestionnaire wrapMsg appState model =
    case model.questionnaireToBeDeleted of
        Just questionnaire ->
            let
                newModel =
                    { model | deletingQuestionnaire = Loading }

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.deleteQuestionnaire questionnaire.uuid appState DeleteQuestionnaireCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


deleteQuestionnaireCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteQuestionnaireCompleted wrapMsg appState model result =
    case result of
        Ok user ->
            ( { model | deletingQuestionnaire = Success "Questionnaire was sucessfully deleted", questionnaires = Loading, questionnaireToBeDeleted = Nothing }
            , fetchData wrapMsg appState
            )

        Err error ->
            ( { model | deletingQuestionnaire = getServerError error "Questionnaire could not be deleted" }
            , getResultCmd result
            )


handleDeleteMigration : (Msg -> Msgs.Msg) -> AppState -> String -> Model -> (Model, Cmd Msgs.Msg)
handleDeleteMigration wrapMsg appState uuid model =
    ( { model | deletingMigration = Loading }, deletingMigrationCmd wrapMsg appState uuid )


deleteMigrationCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteMigrationCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | deletingMigration = Success "Questionnaire migration was canceled", questionnaires = Loading }
            , fetchData wrapMsg appState
            )
        Err error ->
            ( { model | deletingMigration = getServerError error "Questionnaire migration could not be canceled" }
            , getResultCmd result
            )


deletingMigrationCmd : (Msg -> Msgs.Msg) -> AppState -> String -> Cmd Msgs.Msg
deletingMigrationCmd wrapMsg appState uuid =
    Cmd.map wrapMsg
        <| QuestionnairesApi.deleteQuestionnaireMigration uuid appState DeleteQuestionnaireMigrationCompleted


handleQuestionnaireUpgradeForm : (Msg -> Msgs.Msg) -> AppState -> Form.Msg -> Model -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireUpgradeForm wrapMsg appState formMsg model =
    let
        body form =
            encodeQuestionnaireUpgradeForm form

        createMigrationCmd form uuid =
            Cmd.map wrapMsg
                <| QuestionnairesApi.postQuestionnaireMigration uuid (body form) appState PostQuestionnaireMigrationCompleted
    in
    case ( formMsg, Form.getOutput model.questionnaireUpgradeForm, model.questionnaireToBeUpgraded ) of
        ( Form.Submit, Just questionnaireForm, Just questionnaire) ->
            ( { model | creatingQuestionnaireMigration = Loading }
            , createMigrationCmd questionnaireForm questionnaire.uuid
            )

        _ ->
            ( { model | questionnaireUpgradeForm = Form.update questionnaireUpgradeFormValidation formMsg model.questionnaireUpgradeForm }
            , Cmd.none
            )


handlePostQuestionnaireMigrationCompleted : (Msg -> Msgs.Msg) -> AppState -> Result ApiError () -> Model -> ( Model, Cmd Msgs.Msg )
handlePostQuestionnaireMigrationCompleted wrapMsg appState result model =
    case result of
        Ok _ ->
            let
                questionnaireUuid =
                    model.questionnaireToBeUpgraded
                        |> Maybe.andThen (Just << .uuid)
                        |> Maybe.withDefault ""
            in
            ( model, cmdNavigate appState.key <| Routing.Questionnaires << Migrate <| questionnaireUuid)

        Err error ->
            ( { model | creatingQuestionnaireMigration = getServerError error "Questionnaire migration could not be created." }
            , getResultCmd result
            )


handleUpgradableKnowledgeModelsCompleted : (Msg -> Msgs.Msg) -> Result ApiError (List PackageDetail) -> Model -> ( Model, Cmd Msgs.Msg )
handleUpgradableKnowledgeModelsCompleted wrapMsg result model =
    case result of
        Ok kms ->
            let
                currentVersion =
                    model.questionnaireToBeUpgraded
                        |> Maybe.map (.version << .package)
                        |> Maybe.withDefault ""

                packages =
                    List.filter (.version >> versionIsGreater currentVersion) kms

            in
            ( { model | upgradableKnowledgeModels = Success packages }
            , Cmd.none
            )

        Err error ->
            ( { model | upgradableKnowledgeModels = getServerError error "Unable to get knowledge models list" }
            , getResultCmd result
            )


handleShowHideQuestionnaireUpgradeForm : (Msg -> Msgs.Msg) -> AppState -> Maybe Questionnaire -> Model -> (Model, Cmd Msgs.Msg)
handleShowHideQuestionnaireUpgradeForm wrapMsg appState mQuestionnaure model =
    case mQuestionnaure of
        Just questionnaire ->
            let
                pkg =
                    questionnaire.package

                cmd =
                    Cmd.map wrapMsg
                        <| PackagesApi.getPackagesFiltered pkg.organizationId pkg.kmId appState GetUpgradableKnowledgeModelsCompleted
            in
            ( { model | questionnaireToBeUpgraded = mQuestionnaure, upgradableKnowledgeModels = Loading }, cmd )

        _ ->
            ( { model | questionnaireToBeUpgraded = Nothing, upgradableKnowledgeModels = Unset}, Cmd.none )
