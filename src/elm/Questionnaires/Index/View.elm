module Questionnaires.Index.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa, linkTo)
import Common.Html.Attribute exposing (listClass)
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Common.View.Modal as Modal
import Common.View.Page as Page
import Form
import Html exposing (..)
import Html.Attributes exposing (..)
import KnowledgeModels.Common.Models exposing (PackageDetail)
import KnowledgeModels.Routing
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire, QuestionnaireState(..), isEditable)
import Questionnaires.Common.View exposing (accessibilityBadge)
import Questionnaires.Index.ExportModal.View as ExportModal
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing
import Utils exposing (listInsertIf)


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (viewQuestionnaires wrapMsg appState model) model.questionnaires


viewQuestionnaires : (Msg -> Msgs.Msg) -> AppState -> Model -> List Questionnaire -> Html Msgs.Msg
viewQuestionnaires wrapMsg appState model questionnaires =
    div [ listClass "Questionnaires__Index" ]
        [ Page.header "Questionnaires" indexActions
        , FormResult.successOnlyView model.deletingQuestionnaire
        , Listing.view (listingConfig wrapMsg appState) <| List.sortBy .name questionnaires
        , ExportModal.view (wrapMsg << ExportModalMsg) appState model.exportModalModel
        , deleteModal wrapMsg model
        , upgradeQuestionnaireModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.Questionnaires <| Create Nothing) [ class "btn btn-primary" ] [ text "Create" ] ]


listingConfig : (Msg -> Msgs.Msg) -> AppState -> ListingConfig Questionnaire Msgs.Msg
listingConfig wrapMsg appState =
    { title = listingTitle appState
    , description = listingDescription
    , actions = listingActions wrapMsg appState
    , textTitle = .name
    , emptyText = "Click \"Create\" button to add a new Questionnaire."
    }


listingTitle : AppState -> Questionnaire -> Html Msgs.Msg
listingTitle appState questionnaire =
    span []
        [ linkTo (detailRoute questionnaire) [] [ text questionnaire.name ]
        , ownerIcon appState questionnaire
        , accessibilityBadge questionnaire.accessibility
        , migrationBadge questionnaire.state
        ]


ownerIcon : AppState -> Questionnaire -> Html Msgs.Msg
ownerIcon appState questionnaire =
    if questionnaire.ownerUuid == Maybe.map .uuid appState.session.user then
        i [ class "fa fa-user", title "Questionnaire created by you" ] []

    else
        emptyNode


listingDescription : Questionnaire -> Html Msgs.Msg
listingDescription questionnaire =
    let
        kmRoute =
            Routing.KnowledgeModels <|
                KnowledgeModels.Routing.Detail
                    questionnaire.package.organizationId
                    questionnaire.package.kmId
    in
    linkTo kmRoute
        [ title "Knowledge Model" ]
        [ text questionnaire.package.name
        , text ", "
        , text questionnaire.package.version
        , text " ("
        , code [] [ text questionnaire.package.id ]
        , text ")"
        ]


listingActions : (Msg -> Msgs.Msg) -> AppState -> Questionnaire -> List (ListingActionConfig Msgs.Msg)
listingActions wrapMsg appState questionnaire =
    let
        fillQuestionnaire =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "Fill questionnaire"
            , msg = ListingActionLink (detailRoute questionnaire)
            }

        viewQuestionnaire =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "View questionnaire"
            , msg = ListingActionLink (detailRoute questionnaire)
            }

        export_ =
            { extraClass = Nothing
            , icon = Just "download"
            , label = "Export"
            , msg = ListingActionMsg (wrapMsg <| ShowExportQuestionnaire questionnaire)
            }

        upgrade =
            { extraClass = Nothing
            , icon = Just "angle-double-up"
            , label = "Upgrade"
            , msg = ListingActionMsg (wrapMsg <| ShowHideQuestionnaireUpgradeForm (Just questionnaire))
            }

        continueMigration =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "Continue Migration"
            , msg = ListingActionLink (Routing.Questionnaires <| Migration <| questionnaire.uuid)
            }

        cancelMigration =
            { extraClass = Nothing
            , icon = Just "ban"
            , label = "Cancel Migration"
            , msg = ListingActionMsg (wrapMsg <| DeleteQuestionnaireMigration questionnaire.uuid)
            }

        edit =
            { extraClass = Nothing
            , icon = Just "edit"
            , label = "Edit"
            , msg = ListingActionLink (Routing.Questionnaires <| Edit <| questionnaire.uuid)
            }

        delete =
            { extraClass = Just "text-danger"
            , icon = Just "trash-o"
            , label = "Delete"
            , msg = ListingActionMsg (wrapMsg <| ShowHideDeleteQuestionnaire <| Just questionnaire)
            }

        editable =
            isEditable appState questionnaire

        migrating =
            questionnaire.state == Migrating

        outdated =
            questionnaire.state == Outdated
    in
    []
        |> listInsertIf fillQuestionnaire (editable && not migrating)
        |> listInsertIf viewQuestionnaire (not editable && not migrating)
        |> listInsertIf continueMigration migrating
        |> listInsertIf cancelMigration migrating
        |> listInsertIf export_ True
        |> listInsertIf upgrade outdated
        |> listInsertIf edit editable
        |> listInsertIf delete editable


detailRoute : Questionnaire -> Routing.Route
detailRoute =
    Routing.Questionnaires << Detail << .uuid


deleteModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteModal wrapMsg model =
    let
        ( visible, name ) =
            case model.questionnaireToBeDeleted of
                Just questionnaire ->
                    ( True, questionnaire.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text name ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete questionnaire"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingQuestionnaire
            , actionName = "Delete"
            , actionMsg = wrapMsg DeleteQuestionnaire
            , cancelMsg = Just <| wrapMsg <| ShowHideDeleteQuestionnaire Nothing
            }
    in
    Modal.confirm modalConfig


upgradeQuestionnaireModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
upgradeQuestionnaireModal wrapMsg model =
    let
        ( visible, name ) =
            case model.questionnaireToBeUpgraded of
                Just questionnaire ->
                    ( True, questionnaire.name )

                Nothing ->
                    ( False, "" )

        options =
            case model.upgradableKnowledgeModels of
                Success packages ->
                    ( "", "- select new base knowledge model -" ) :: List.map upgradeOption packages

                _ ->
                    []

        modalContent : List (Html Msgs.Msg)
        modalContent =
            case model.upgradableKnowledgeModels of
                Unset ->
                    [ emptyNode ]

                Loading ->
                    [ Page.loader ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success packages ->
                    [ p [ class "alert alert-info" ]
                        [ text "Select new knowledge model base for "
                        , strong [] [ text name ]
                        , text " questionnaire."
                        ]
                    , FormGroup.select options model.questionnaireUpgradeForm "targetPackageId" "New knowledge model base"
                        |> Html.map (wrapMsg << UpgradeQuestionnaireForm)
                    ]

        modalConfig =
            { modalTitle = "Create migration"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.creatingQuestionnaireMigration
            , actionName = "Create"
            , actionMsg = wrapMsg <| UpgradeQuestionnaireForm Form.Submit
            , cancelMsg = Just <| wrapMsg <| ShowHideQuestionnaireUpgradeForm Nothing
            }
    in
    Modal.confirm modalConfig


migrationBadge : QuestionnaireState -> Html msg
migrationBadge state =
    case state of
        Migrating ->
            span [ class "badge badge-info" ]
                [ text "migrating" ]

        Outdated ->
            span [ class "badge badge-warning" ]
                [ text "outdated" ]

        Default ->
            emptyNode


upgradeOption : PackageDetail -> ( String, String )
upgradeOption package =
    let
        optionText =
            package.name ++ " " ++ package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
