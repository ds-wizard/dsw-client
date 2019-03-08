module Questionnaires.Subscriptions exposing (subscriptions)

import Msgs
import Questionnaires.Migration.Subscriptions
import Questionnaires.Models exposing (Model)
import Questionnaires.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))


subscriptions : (Msg -> Msgs.Msg) -> Route -> Model -> Sub Msgs.Msg
subscriptions wrapMsg route model =
    case route of
        Migrate _ ->
            Questionnaires.Migration.Subscriptions.subscriptions (wrapMsg << MigrationMsg) model.migrationModel

        _ ->
            Sub.none
