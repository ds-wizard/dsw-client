module KnowledgeModels.Import.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Json.Decode as Decode
import Jwt
import KnowledgeModels.Import.Models exposing (Model, dropzoneId, fileInputId)
import KnowledgeModels.Import.Msgs exposing (Msg(..))
import KnowledgeModels.Requests exposing (importPackage)
import KnowledgeModels.Routing
import Models exposing (State)
import Msgs
import Ports exposing (FilePortData, createDropzone, fileSelected)
import Requests exposing (getResultCmd)
import Routing exposing (Route(..), cmdNavigate)


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        DragEnter ->
            ( { model | dnd = model.dnd + 1 }, createDropzone dropzoneId )

        DragLeave ->
            ( { model | dnd = model.dnd - 1 }, Cmd.none )

        FileSelected ->
            ( model, fileSelected fileInputId )

        FileRead data ->
            ( { model | file = Just data }, Cmd.none )

        Submit ->
            handleSubmit wrapMsg state.session model

        Cancel ->
            ( { model | file = Nothing, importing = Unset, dnd = 0 }, Cmd.none )

        ImportPackageCompleted result ->
            importPackageCompleted state model result

        _ ->
            ( model, Cmd.none )


handleSubmit : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleSubmit wrapMsg session model =
    case model.file of
        Just file ->
            ( { model | importing = Loading }, importPackageCmd wrapMsg file session )

        Nothing ->
            ( model, Cmd.none )


importPackageCmd : (Msg -> Msgs.Msg) -> FilePortData -> Session -> Cmd Msgs.Msg
importPackageCmd wrapMsg file session =
    importPackage file session
        |> Jwt.send ImportPackageCompleted
        |> Cmd.map wrapMsg


importPackageCompleted : State -> Model -> Result Jwt.JwtError Decode.Value -> ( Model, Cmd Msgs.Msg )
importPackageCompleted state model result =
    case result of
        Ok msg ->
            ( model, cmdNavigate state.key (KnowledgeModels KnowledgeModels.Routing.Index) )

        Err error ->
            ( { model | importing = getServerErrorJwt error "Importing package failed." }
            , getResultCmd result
            )