module Common.Api.SHACLs exposing (fetchPreview)

import Common.Api exposing (ToMsg, jwtFetchWithString)
import Common.AppState exposing (AppState)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)


fetchPreview : String -> AppState -> ToMsg KnowledgeModel msg -> Cmd msg
fetchPreview shacl =
    jwtFetchWithString "/shacls/preview" "text/turtle" shacl KnowledgeModel.decoder
