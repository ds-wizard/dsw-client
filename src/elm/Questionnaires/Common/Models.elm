module Questionnaires.Common.Models exposing
    ( Questionnaire
    , QuestionnaireMigration
    , QuestionnaireState(..)
    , isEditable
    , questionnaireDecoder
    , questionnaireListDecoder
    , questionnaireMigrationDecoder
    )


import Auth.Role as Role
import Common.AppState exposing (AppState)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, questionnaireDetailDecoder)
import KMEditor.Common.Models.Events exposing (Event, eventDecoder)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, knowledgeModelDecoder)
import KnowledgeModels.Common.Models exposing (PackageDetail, packageDetailDecoder)
import Questionnaires.Common.Models.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))


type alias Questionnaire =
    { uuid : String
    , name : String
    , package : PackageDetail
    , level : Int
    , accessibility : QuestionnaireAccessibility
    , ownerUuid : Maybe String
    , state : QuestionnaireState
    }


type alias QuestionnaireMigration =
    { questionnaire : QuestionnaireDetail
    , diffEvents : List Event
    , diffKnowledgeModel : KnowledgeModel
    , previousKnowledgeModel : KnowledgeModel
    , targetPackageId : String
    }


type QuestionnaireState
    = Default
    | Outdated
    | Migrating


isEditable :
    AppState
    ->
        { a
            | accessibility : QuestionnaireAccessibility
            , ownerUuid : Maybe String
        }
    -> Bool
isEditable appState questionnaire =
    let
        isAdmin =
            Role.isAdmin appState.session.user

        isNotReadonly =
            questionnaire.accessibility /= PublicReadOnlyQuestionnaire

        isOwner =
            questionnaire.ownerUuid == Maybe.map .uuid appState.session.user
    in
    isAdmin || isNotReadonly || isOwner


questionnaireDecoder : Decoder Questionnaire
questionnaireDecoder =
    Decode.succeed Questionnaire
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" packageDetailDecoder
        |> optional "level" Decode.int 0
        |> required "accessibility" QuestionnaireAccessibility.decoder
        |> required "ownerUuid" (Decode.maybe Decode.string)
        |> required "state" questionnaireStateDecoder


questionnaireListDecoder : Decoder (List Questionnaire)
questionnaireListDecoder =
    Decode.list questionnaireDecoder


questionnaireMigrationDecoder : Decoder QuestionnaireMigration
questionnaireMigrationDecoder =
    Decode.succeed QuestionnaireMigration
        |> required "questionnaire" questionnaireDetailDecoder
        |> required "diffEvents" (Decode.list eventDecoder)
        |> required "diffKnowledgeModel" knowledgeModelDecoder
        |> required "previousKnowledgeModel" knowledgeModelDecoder
        |> required "targetPackageId" Decode.string


questionnaireStateDecoder : Decoder QuestionnaireState
questionnaireStateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Default" ->
                        Decode.succeed Default

                    "Outdated" ->
                        Decode.succeed Outdated

                    "Migrating" ->
                        Decode.succeed Migrating

                    unknownState ->
                        Decode.fail <| "Unknown questionnaire state " ++ unknownState
            )
