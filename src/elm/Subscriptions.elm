module Subscriptions exposing (subscriptions)

import Common.Menu.Subscriptions
import KMEditor.Subscriptions
import KnowledgeModels.Subscriptions
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Questionnaires.Subscriptions
import Routing exposing (Route(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        currentViewSubscriptions =
            case model.appState.route of
                KMEditor route ->
                    KMEditor.Subscriptions.subscriptions KMEditorMsg route model.kmEditorModel

                KnowledgeModels route ->
                    KnowledgeModels.Subscriptions.subscriptions KnowledgeModelsMsg route model.kmPackagesModel

                Questionnaires route ->
                    Questionnaires.Subscriptions.subscriptions QuestionnairesMsg route model.dsPlannerModel

                _ ->
                    Sub.none

        menuSubscriptions =
            Common.Menu.Subscriptions.subscriptions model.menuModel
    in
    Sub.batch [ currentViewSubscriptions, menuSubscriptions ]
