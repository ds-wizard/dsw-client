module Common.Api.Packages exposing
    ( deletePackage
    , deletePackageVersion
    , exportPackageUrl
    , getPackage
    , getPackages
    , importPackage
    , pullPackage
    )

import Common.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPostEmpty, jwtPostFile)
import Common.AppState exposing (AppState)
import File exposing (File)
import Json.Decode as D
import KnowledgeModels.Common.Package as Package exposing (Package)
import KnowledgeModels.Common.PackageDetail as PackageDetail exposing (PackageDetail)


getPackages : AppState -> ToMsg (List Package) msg -> Cmd msg
getPackages =
    jwtGet "/packages" (D.list Package.decoder)


getPackage : String -> AppState -> ToMsg PackageDetail msg -> Cmd msg
getPackage packageId =
    jwtGet ("/packages/" ++ packageId) PackageDetail.decoder


deletePackage : String -> String -> AppState -> ToMsg () msg -> Cmd msg
deletePackage organizationId kmId =
    jwtDelete ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deletePackageVersion : String -> AppState -> ToMsg () msg -> Cmd msg
deletePackageVersion packageId =
    jwtDelete ("/packages/" ++ packageId)


pullPackage : String -> AppState -> ToMsg () msg -> Cmd msg
pullPackage packageId =
    jwtPostEmpty ("/packages/" ++ packageId ++ "/pull")


importPackage : File -> AppState -> ToMsg (List Package) msg -> Cmd msg
importPackage file =
    jwtPostFile "/import" file (D.list Package.decoder)


exportPackageUrl : String -> AppState -> String
exportPackageUrl packageId appState =
    appState.apiUrl ++ "/export/" ++ packageId
