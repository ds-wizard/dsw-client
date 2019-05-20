module Questionnaires.Migration.Subscriptions exposing (subscriptions)

import Msgs
import Questionnaires.Migration.Models exposing (Model)
import Questionnaires.Migration.Msgs exposing (Msg(..))
import SplitPane


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    SplitPane.subscriptions model.splitPaneState |> Sub.map (wrapMsg << SplitPaneMsg)
