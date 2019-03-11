module KMEditor.Editor.KMEditor.Update.Tag exposing
    ( deleteTag
    , removeTag
    , updateTagForm
    , withGenerateTagEditEvent
    )

import Form
import KMEditor.Editor.KMEditor.Models exposing (Model)
import KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), KMEditorData, QuestionEditorData, TagEditorData, isTagEditorDirty, updateTagEditorData)
import KMEditor.Editor.KMEditor.Models.Forms exposing (tagFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createAddTagEvent, createDeleteTagEvent, createEditTagEvent)
import Msgs
import Random exposing (Seed)


updateTagForm : Model -> Form.Msg -> TagEditorData -> Model
updateTagForm =
    updateForm
        { formValidation = tagFormValidation
        , createEditor = TagEditor
        }


withGenerateTagEditEvent : Seed -> Model -> TagEditorData -> (Seed -> Model -> TagEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateTagEditEvent =
    withGenerateEvent
        { isDirty = isTagEditorDirty
        , formValidation = tagFormValidation
        , createEditor = TagEditor
        , alert = "Please fix the tag errors first."
        , createAddEvent = createAddTagEvent
        , createEditEvent = createEditTagEvent
        , updateEditorData = updateTagEditorData
        , updateEditors = Nothing
        }


deleteTag : Seed -> Model -> String -> TagEditorData -> ( Seed, Model )
deleteTag =
    deleteEntity
        { removeEntity = removeTag
        , createEditor = TagEditor
        , createDeleteEvent = createDeleteTagEvent
        }


removeTag : (String -> Children -> Children) -> String -> Editor -> Editor
removeTag removeFn uuid =
    updateIfKMEditor (\data -> { data | tags = removeFn uuid data.tags })


updateIfKMEditor : (KMEditorData -> KMEditorData) -> Editor -> Editor
updateIfKMEditor update editor =
    case editor of
        KMEditor kmEditorData ->
            KMEditor <| update kmEditorData

        _ ->
            editor
