module Questionnaires.Routing exposing (Route(..), isAllowed, moduleRoot, parses, toUrl)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Url.Parser exposing (..)
import Url.Parser.Query as Query


type Route
    = Create (Maybe String)
    | Detail String
    | Edit String
    | Index
    | Migrate String


moduleRoot : String
moduleRoot =
    "questionnaires"


parses : (Route -> a) -> List (Parser (a -> c) c)
parses wrapRoute =
    [ map (wrapRoute << Create) (s moduleRoot </> s "create" <?> Query.string "selected")
    , map (wrapRoute << Detail) (s moduleRoot </> s "detail" </> string)
    , map (wrapRoute << Edit) (s moduleRoot </> s "edit" </> string)
    , map (wrapRoute <| Index) (s moduleRoot)
    , map (wrapRoute << Migrate) (s moduleRoot </> string </> s "migration")
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        Create selected ->
            case selected of
                Just id ->
                    [ moduleRoot, "create", "?selected=" ++ id ]

                Nothing ->
                    [ moduleRoot, "create" ]

        Detail uuid ->
            [ moduleRoot, "detail", uuid ]

        Edit uuid ->
            [ moduleRoot, "edit", uuid ]

        Index ->
            [ moduleRoot ]

        Migrate uuid ->
            [ moduleRoot, uuid, "migration" ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    hasPerm maybeJwt Perm.questionnaire
