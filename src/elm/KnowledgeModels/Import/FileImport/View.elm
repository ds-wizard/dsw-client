module KnowledgeModels.Import.FileImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Locale exposing (l, lx)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import KnowledgeModels.Import.FileImport.Models exposing (..)
import KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "KnowledgeModels.Import.FileImport.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "KnowledgeModels.Import.FileImport.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.file of
                Just file ->
                    fileView appState model file.filename

                Nothing ->
                    dropzone appState model
    in
    div [ class "KnowledgeModels__Import__FileImport", id dropzoneId ]
        [ FormResult.view model.importing
        , content
        ]


fileView : AppState -> Model -> String -> Html Msg
fileView appState model fileName =
    let
        cancelDisabled =
            case model.importing of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "file-view" ]
        [ div [ class "file" ]
            [ i [ class "fa fa-file-o" ] []
            , div [ class "filename" ]
                [ text fileName ]
            ]
        , div [ class "actions" ]
            [ button [ disabled cancelDisabled, onClick Cancel, class "btn btn-secondary" ]
                [ lx_ "fileView.cancel" appState ]
            , ActionButton.button <| ActionButton.ButtonConfig (l_ "fileView.upload" appState) model.importing Submit False
            ]
        ]


dropzone : AppState -> Model -> Html Msg
dropzone appState model =
    div (dropzoneAttributes model)
        [ label [ class "btn btn-secondary btn-file" ]
            [ lx_ "dropzone.choose" appState
            , input [ id fileInputId, type_ "file", on "change" (Decode.succeed FileSelected) ] []
            ]
        , p [] [ lx_ "dropzone.drop" appState ]
        ]


dropzoneAttributes : Model -> List (Attribute Msg)
dropzoneAttributes model =
    let
        cssClass =
            case model.dnd of
                0 ->
                    ""

                _ ->
                    "active"
    in
    [ class ("dropzone " ++ cssClass)
    , id dropzoneId
    , onDragEvent "dragenter" DragEnter
    , onDragEvent "dragover" DragOver
    , onDragEvent "dragleave" DragLeave
    ]


onDragEvent : String -> Msg -> Attribute Msg
onDragEvent event msg =
    custom event <|
        Decode.succeed
            { stopPropagation = True
            , preventDefault = True
            , message = msg
            }
