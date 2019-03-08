module Common.Api.Questionnaires exposing (deleteQuestionnaire, exportQuestionnaireUrl, fetchSummaryReport, getQuestionnaire, getQuestionnairePublic, getQuestionnaires, getQuestionnaireMigration, postQuestionnaire, postQuestionnaireMigration, putQuestionnaire, putMigrateQuestionnaire, putQuestionnaireMigrationQuestionFlag, deleteQuestionnaireMigration)

import Common.Api exposing (ToMsg, httpGet, jwtDelete, jwtFetch, jwtGet, jwtPost, jwtPut)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, questionnaireDetailDecoder)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport, summaryReportDecoder)
import Json.Encode exposing (Value)
import Questionnaires.Common.Models exposing (Questionnaire, QuestionnaireMigration, questionnaireDecoder, questionnaireListDecoder, questionnaireMigrationDecoder)


emptyValue : Value
emptyValue =
    Json.Encode.string ""

getQuestionnaires : AppState -> ToMsg (List Questionnaire) msg -> Cmd msg
getQuestionnaires =
    jwtGet "/questionnaires" questionnaireListDecoder


getQuestionnaire : String -> AppState -> ToMsg QuestionnaireDetail msg -> Cmd msg
getQuestionnaire uuid =
    jwtGet ("/questionnaires/" ++ uuid) questionnaireDetailDecoder


getQuestionnaireMigration : String -> AppState -> ToMsg QuestionnaireMigration msg -> Cmd msg
getQuestionnaireMigration uuid =
    jwtGet ("/questionnaires/" ++ uuid ++ "/migrations") questionnaireMigrationDecoder


getQuestionnairePublic : AppState -> ToMsg QuestionnaireDetail msg -> Cmd msg
getQuestionnairePublic =
    httpGet "/questionnaires/public" questionnaireDetailDecoder


postQuestionnaire : Value -> AppState -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaire =
    jwtFetch "/questionnaires" questionnaireDecoder


postQuestionnaireMigration : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
postQuestionnaireMigration uuid =
    jwtPost ("/questionnaires/" ++ uuid ++ "/migrations")


putQuestionnaire : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putQuestionnaire uuid =
    jwtPut ("/questionnaires/" ++ uuid)


putQuestionnaireMigrationQuestionFlag : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putQuestionnaireMigrationQuestionFlag uuid =
    jwtPut ("/questionnaires/" ++ uuid ++ "/migrations/resolveQuestionEvent")


putMigrateQuestionnaire : String -> AppState -> ToMsg () msg -> Cmd msg
putMigrateQuestionnaire uuid =
    jwtPut ("/questionnaires/" ++ uuid ++ "/migrations") emptyValue


deleteQuestionnaire : String -> AppState -> ToMsg () msg -> Cmd msg
deleteQuestionnaire uuid =
    jwtDelete ("/questionnaires/" ++ uuid)


fetchSummaryReport : String -> Value -> AppState -> ToMsg SummaryReport msg -> Cmd msg
fetchSummaryReport questionnaireUuid =
    jwtFetch ("/questionnaires/" ++ questionnaireUuid ++ "/report/preview") summaryReportDecoder


exportQuestionnaireUrl : String -> String -> Maybe String -> AppState -> String
exportQuestionnaireUrl uuid format templateUuid appState =
    let
        url =
            appState.apiUrl ++ "/questionnaires/" ++ uuid ++ "/dmp?format=" ++ format
    in
    templateUuid
        |> Maybe.map ((++) (url ++ "&templateUuid="))
        |> Maybe.withDefault url


deleteQuestionnaireMigration : String -> AppState -> ToMsg () msg -> Cmd msg
deleteQuestionnaireMigration uuid =
    jwtDelete ("/questionnaires/" ++ uuid ++ "/migrations")
