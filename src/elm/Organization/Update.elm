module Organization.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Organizations as OrganizationsApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (lg)
import Form exposing (Form)
import Msgs
import Organization.Common.Organization exposing (Organization)
import Organization.Common.OrganizationForm as OrganizationForm
import Organization.Models exposing (..)
import Organization.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    OrganizationsApi.getCurrentOrganization appState GetCurrentOrganizationCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetCurrentOrganizationCompleted result ->
            getCurrentOrganizationCompleted appState model result

        PutCurrentOrganizationCompleted result ->
            putCurrentOrganizationCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


getCurrentOrganizationCompleted : AppState -> Model -> Result ApiError Organization -> ( Model, Cmd Msgs.Msg )
getCurrentOrganizationCompleted appState model result =
    let
        newModel =
            case result of
                Ok organization ->
                    { model | form = OrganizationForm.init organization, organization = Success organization }

                Err error ->
                    { model | organization = getServerError error <| lg "apiError.organizations.getError" appState }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


putCurrentOrganizationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
putCurrentOrganizationCompleted appState model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.organizations.put" appState

                Err error ->
                    getServerError error <| lg "apiError.organizations.putError" appState

        cmd =
            getResultCmd result
    in
    ( { model | savingOrganization = newResult }, cmd )


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.organization ) of
        ( Form.Submit, Just form, Success organization ) ->
            let
                body =
                    OrganizationForm.encode organization.uuid form

                cmd =
                    Cmd.map wrapMsg <|
                        OrganizationsApi.putCurrentOrganization body appState PutCurrentOrganizationCompleted
            in
            ( { model | savingOrganization = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update OrganizationForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
