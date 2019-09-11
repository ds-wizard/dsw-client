module KnowledgeModels.Import.FileImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Locale exposing (l, lx)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder)
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
                    fileView appState model <| File.name file

                Nothing ->
                    dropzone appState model
    in
    div [ class "KnowledgeModels__Import__FileImport" ]
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
        [ button [ class "btn btn-secondary", onClick PickFiles ] [ lx_ "dropzone.choose" appState ]
        , p [] [ lx_ "dropzone.drop" appState ]
        ]


dropzoneAttributes : Model -> List (Attribute Msg)
dropzoneAttributes model =
    [ class "dropzone"
    , classList [ ( "active", model.hover ) ]
    , hijackOn "dragenter" (D.succeed DragEnter)
    , hijackOn "dragover" (D.succeed DragEnter)
    , hijackOn "dragleave" (D.succeed DragLeave)
    , hijackOn "drop" dropDecoder
    ]


dropDecoder : Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
