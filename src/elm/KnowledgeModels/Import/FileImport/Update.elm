module KnowledgeModels.Import.FileImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (lg)
import File.Select as Select
import KnowledgeModels.Common.Package exposing (Package)
import KnowledgeModels.Import.FileImport.Models exposing (Model)
import KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))
import KnowledgeModels.Routes exposing (Route(..))
import Msgs
import Routes
import Routing exposing (cmdNavigate)


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        PickFiles ->
            ( model, Cmd.map wrapMsg <| Select.files [] GotFiles )

        GotFiles file _ ->
            ( { model | file = Just file }, Cmd.none )

        Submit ->
            handleSubmit wrapMsg appState model

        Cancel ->
            ( { model | file = Nothing, importing = Unset, hover = False }, Cmd.none )

        ImportPackageCompleted result ->
            importPackageCompleted appState model result


handleSubmit : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleSubmit wrapMsg appState model =
    case model.file of
        Just file ->
            ( { model | importing = Loading }
            , Cmd.map wrapMsg <| PackagesApi.importPackage file appState ImportPackageCompleted
            )

        Nothing ->
            ( model, Cmd.none )


importPackageCompleted : AppState -> Model -> Result ApiError (List Package) -> ( Model, Cmd Msgs.Msg )
importPackageCompleted appState model result =
    case result of
        Ok packages ->
            let
                route =
                    List.foldl (always << Just) Nothing packages
                        |> Maybe.map (Routes.KnowledgeModelsRoute << DetailRoute << .id)
                        |> Maybe.withDefault (Routes.KnowledgeModelsRoute IndexRoute)
            in
            ( model, cmdNavigate appState route )

        Err error ->
            ( { model | importing = getServerError error <| lg "apiError.packages.importError" appState }
            , getResultCmd result
            )
