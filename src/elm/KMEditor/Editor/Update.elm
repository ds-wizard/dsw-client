module KMEditor.Editor.Update exposing (getKnowledgeModelCmd, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMEditor.Editor.Models exposing (..)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Entities exposing (..)
import KMEditor.Editor.Models.Events exposing (..)
import KMEditor.Editor.Msgs exposing (..)
import KMEditor.Editor.Update.UpdateAnswer exposing (..)
import KMEditor.Editor.Update.UpdateChapter exposing (..)
import KMEditor.Editor.Update.UpdateExpert exposing (..)
import KMEditor.Editor.Update.UpdateKnowledgeModel exposing (..)
import KMEditor.Editor.Update.UpdateQuestion as UpdateQuestion
import KMEditor.Editor.Update.UpdateReference exposing (..)
import KMEditor.Editor.Update.Utils exposing (..)
import KMEditor.Requests exposing (getKnowledgeModelData, postEventsBulk)
import Msgs
import Random.Pcg exposing (Seed)
import Reorderable
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (getUuid, tuplePrepend)


update : Msg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg seed session model =
    case msg of
        GetKnowledgeModelCompleted result ->
            getKnowledgeModelCompleted model result |> tuplePrepend seed

        Edit knowledgeModelMsg ->
            updateEdit knowledgeModelMsg seed session model

        SaveCompleted result ->
            postEventsBulkCompleted model result |> tuplePrepend seed

        ReorderableMsg reorderableMsg ->
            let
                newReorderableState =
                    Reorderable.update reorderableMsg model.reorderableState
            in
            ( seed, { model | reorderableState = newReorderableState }, Cmd.none )


getKnowledgeModelCmd : String -> Session -> Cmd Msgs.Msg
getKnowledgeModelCmd uuid session =
    getKnowledgeModelData uuid session
        |> toCmd GetKnowledgeModelCompleted Msgs.KnowledgeModelsEditorMsg


getKnowledgeModelCompleted : Model -> Result Jwt.JwtError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelEditor = Success <| createKnowledgeModelEditor knowledgeModel }

                Err error ->
                    { model | knowledgeModelEditor = getServerErrorJwt error "Unable to get knowledge model" }
    in
    ( newModel, Cmd.none )


postEventsBulkCmd : String -> List Event -> Session -> Cmd Msgs.Msg
postEventsBulkCmd uuid events session =
    encodeEvents events
        |> postEventsBulk session uuid
        |> toCmd SaveCompleted Msgs.KnowledgeModelsEditorMsg


postEventsBulkCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postEventsBulkCompleted model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate KMEditor )

        Err error ->
            ( { model | saving = getServerErrorJwt error "Knowledge model could not be saved" }, Cmd.none )


updateEdit : KnowledgeModelMsg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
updateEdit msg seed session model =
    case model.knowledgeModelEditor of
        Success knowledgeModelEditor ->
            let
                ( newSeed, newKnowledgeModelEditor, maybeEvent, submit ) =
                    updateKnowledgeModel msg seed knowledgeModelEditor

                newEvents =
                    case maybeEvent of
                        Just event ->
                            model.events ++ [ event ]

                        Nothing ->
                            model.events

                newReorderableState =
                    Reorderable.update (Reorderable.MouseOverIgnored False) model.reorderableState

                ( newModel, cmd ) =
                    if submit then
                        ( { model | saving = Loading }, postEventsBulkCmd model.branchUuid newEvents session )
                    else
                        ( model, Cmd.none )
            in
            ( newSeed
            , { newModel
                | knowledgeModelEditor = Success newKnowledgeModelEditor
                , events = newEvents
                , reorderableState = newReorderableState
              }
            , cmd
            )

        _ ->
            ( seed, model, Cmd.none )


updateKnowledgeModel : KnowledgeModelMsg -> Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
updateKnowledgeModel msg seed ((KnowledgeModelEditor editor) as kmEditor) =
    case msg of
        KnowledgeModelFormMsg formMsg ->
            updateKnowledgeModelFormMsg formMsg seed kmEditor

        AddChapter ->
            updateKnowledgeModelAddChapter seed kmEditor

        ViewChapter uuid ->
            updateKnowledgeModelViewChapter uuid seed kmEditor

        DeleteChapter uuid ->
            updateKnowledgeModelDeleteChapter uuid seed kmEditor

        ReorderChapterList newChapters ->
            ( seed, KnowledgeModelEditor { editor | chapters = newChapters, chaptersDirty = True }, Nothing, False )

        ChapterMsg uuid chapterMsg ->
            let
                ( newSeed, newChapters, event ) =
                    updateInListWithSeed editor.chapters seed (matchChapter uuid) (updateChapter editor.knowledgeModel chapterMsg)
            in
            ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, event, False )


updateChapter : KnowledgeModel -> ChapterMsg -> Seed -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapter knowledgeModel msg seed ((ChapterEditor editor) as chapterEditor) =
    case msg of
        ChapterFormMsg formMsg ->
            updateChapterFormMsg formMsg seed knowledgeModel chapterEditor

        ChapterCancel ->
            updateChapterCancel seed chapterEditor

        AddChapterQuestion ->
            updateChapterAddQuestion seed knowledgeModel chapterEditor

        ViewQuestion uuid ->
            updateChapterViewQuestion uuid seed chapterEditor

        DeleteChapterQuestion uuid ->
            updateChapterDeleteQuestion uuid seed knowledgeModel chapterEditor

        ReorderQuestionList newQuestions ->
            ( seed, ChapterEditor { editor | questions = newQuestions, questionsDirty = True }, Nothing )

        ChapterQuestionMsg uuid questionMsg ->
            let
                ( newSeed, newQuestions, event ) =
                    updateInListWithSeed editor.questions seed (matchQuestion uuid) (updateQuestion createEditQuestionEvent editor.chapter knowledgeModel questionMsg)
            in
            ( newSeed, ChapterEditor { editor | questions = newQuestions }, event )


updateQuestion : (Chapter -> KnowledgeModel -> Seed -> QuestionEditor -> ( Event, Seed )) -> Chapter -> KnowledgeModel -> QuestionMsg -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestion createEditEvent chapter knowledgeModel msg seed ((QuestionEditor editor) as questionEditor) =
    case msg of
        QuestionFormMsg formMsg ->
            UpdateQuestion.formMsg formMsg seed createEditEvent chapter knowledgeModel questionEditor

        QuestionCancel ->
            UpdateQuestion.cancel seed questionEditor

        AddAnswerItemTemplateQuestion ->
            UpdateQuestion.addAnswerItemTemplateQuestion seed chapter knowledgeModel questionEditor

        ViewAnswerItemTemplateQuestion uuid ->
            UpdateQuestion.viewAnswerItemTemplateQuestion uuid seed questionEditor

        DeleteAnswerItemTemplateQuestion uuid ->
            UpdateQuestion.deleteAnswerItemTemplateQuestion uuid seed chapter knowledgeModel questionEditor

        ReorderAnswerItemTemplateQuestions newAnswerItemTemplateQuestions ->
            ( seed, QuestionEditor { editor | answerItemTemplateQuestions = newAnswerItemTemplateQuestions, answersDirty = True }, Nothing )

        AnswerItemTemplateQuestionMsg uuid answerItemTemplateQuestionMsg ->
            let
                ( newSeed, newAnswerItemTemplateQuestions, event ) =
                    updateInListWithSeed editor.answerItemTemplateQuestions seed (matchQuestion uuid) (updateQuestion (createEditAnswerItemTemplateQuestionEvent editor.question) chapter knowledgeModel answerItemTemplateQuestionMsg)
            in
            ( seed, QuestionEditor { editor | answerItemTemplateQuestions = newAnswerItemTemplateQuestions, answerItemTemplateQuestionsDirty = True }, event )

        AddAnswer ->
            UpdateQuestion.addAnswer seed chapter knowledgeModel questionEditor

        ViewAnswer uuid ->
            UpdateQuestion.viewAnswer uuid seed questionEditor

        DeleteAnswer uuid ->
            UpdateQuestion.deleteAnswer uuid seed chapter knowledgeModel questionEditor

        ReorderAnswerList newAnswers ->
            ( seed, QuestionEditor { editor | answers = newAnswers, answersDirty = True }, Nothing )

        AnswerMsg uuid answerMsg ->
            let
                ( newSeed, newAnswers, event ) =
                    updateInListWithSeed editor.answers seed (matchAnswer uuid) (updateAnswer editor.question chapter knowledgeModel answerMsg)
            in
            ( newSeed, QuestionEditor { editor | answers = newAnswers }, event )

        AddReference ->
            UpdateQuestion.addReference seed chapter knowledgeModel questionEditor

        ViewReference uuid ->
            UpdateQuestion.viewReference uuid seed questionEditor

        DeleteReference uuid ->
            UpdateQuestion.deleteReference uuid seed chapter knowledgeModel questionEditor

        ReorderReferenceList newReferences ->
            ( seed, QuestionEditor { editor | references = newReferences, referencesDirty = True }, Nothing )

        ReferenceMsg uuid referenceMsg ->
            let
                ( newSeed, newReferences, event ) =
                    updateInListWithSeed editor.references seed (matchReference uuid) (updateReference editor.question chapter knowledgeModel referenceMsg)
            in
            ( newSeed, QuestionEditor { editor | references = newReferences }, event )

        AddExpert ->
            UpdateQuestion.addExpert seed chapter knowledgeModel questionEditor

        ViewExpert uuid ->
            UpdateQuestion.viewExpert uuid seed questionEditor

        DeleteExpert uuid ->
            UpdateQuestion.deleteExpert uuid seed chapter knowledgeModel questionEditor

        ReorderExpertList newExperts ->
            ( seed, QuestionEditor { editor | experts = newExperts, expertsDirty = True }, Nothing )

        ExpertMsg uuid expertMsg ->
            let
                ( newSeed, newExperts, event ) =
                    updateInListWithSeed editor.experts seed (matchExpert uuid) (updateExpert editor.question chapter knowledgeModel expertMsg)
            in
            ( newSeed, QuestionEditor { editor | experts = newExperts }, event )


updateAnswer : Question -> Chapter -> KnowledgeModel -> AnswerMsg -> Seed -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
updateAnswer question chapter knowledgeModel msg seed ((AnswerEditor editor) as answerEditor) =
    case msg of
        AnswerFormMsg formMsg ->
            updateAnswerFormMsg formMsg seed question chapter knowledgeModel answerEditor

        AnswerCancel ->
            updateAnswerCancel seed answerEditor

        AddFollowUpQuestion ->
            updateAnswerAddFollowUpQuestion seed chapter knowledgeModel answerEditor

        ViewFollowUpQuestion uuid ->
            updateAnswerViewFollowUpQuestion uuid seed answerEditor

        DeleteFollowUpQuestion uuid ->
            updateAnswerDeleteFollowUpQuestion uuid seed chapter knowledgeModel answerEditor

        ReorderFollowUpQuestionList newFollowUps ->
            ( seed, AnswerEditor { editor | followUps = newFollowUps, followUpsDirty = True }, Nothing )

        FollowUpQuestionMsg uuid questionMsg ->
            let
                ( newSeed, newFollowUps, event ) =
                    updateInListWithSeed editor.followUps seed (matchQuestion uuid) (updateQuestion (createEditFollowUpQuestionEvent editor.answer) chapter knowledgeModel questionMsg)
            in
            ( newSeed, AnswerEditor { editor | followUps = newFollowUps }, event )


updateReference : Question -> Chapter -> KnowledgeModel -> ReferenceMsg -> Seed -> ReferenceEditor -> ( Seed, ReferenceEditor, Maybe Event )
updateReference question chapter knowledgeModel msg seed referenceEditor =
    case msg of
        ReferenceFormMsg formMsg ->
            updateReferenceFormMsg formMsg seed question chapter knowledgeModel referenceEditor

        ReferenceCancel ->
            updateReferenceCancel seed referenceEditor


updateExpert : Question -> Chapter -> KnowledgeModel -> ExpertMsg -> Seed -> ExpertEditor -> ( Seed, ExpertEditor, Maybe Event )
updateExpert question chapter knowledgeModel msg seed expertEditor =
    case msg of
        ExpertFormMsg formMsg ->
            updateExpertFormMsg formMsg seed question chapter knowledgeModel expertEditor

        ExpertCancel ->
            updateExpertCancel seed expertEditor