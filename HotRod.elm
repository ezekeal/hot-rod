module HotRod (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode
import String exposing (toLower)
import PackageJson exposing (PackageJson, parseSemVer)
import Fs exposing (File)


-- MODEL


type alias Model =
  { packageJson : PackageJson
  , error : Maybe String
  }


initialModel : Model
initialModel =
  { packageJson = PackageJson.default
  , error = Nothing
  }


init : ( Model, Effects Action )
init =
  ( initialModel
  , Effects.none
  )



-- UPDATE


type Action
  = NoOp
  | RequestFile String
  | ReceivePackageJson String
  | FileError (Maybe String)
  | CloseError


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    RequestFile filePath ->
      ( model, fetchFile filePath )

    ReceivePackageJson value ->
      ( { model | packageJson = (PackageJson.decode value) }
      , Effects.none
      )

    FileError str ->
      ( { model | error = str }
      , Effects.none
      )

    CloseError ->
      ( { model | error = Nothing }
      , Effects.none
      )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ errorView address model.error
    , div
        [ class "get-package-json" ]
        [ button
            [ onClick address (RequestFile "package.json") ]
            [ text "get package.json" ]
        ]
    , packageJsonView model.packageJson
    ]


errorView : Signal.Address Action -> Maybe String -> Html
errorView address error =
  case error of
    Just value ->
      div
        [ class "error-message"
        , onClick address CloseError
        ]
        [ text value ]

    Nothing ->
      span [] []


packageJsonView : PackageJson -> Html
packageJsonView pj =
  let
    kvDiv key valueView value =
      Maybe.map (valueView >> fieldView key) value

    fields =
      [ Just
            <| h1 [ class "package-title" ]
            <| List.filterMap identity
                [ Maybe.map (spanString "package-name") pj.name
                , Maybe.map (spanString "package-version") pj.version
                ]
      , Maybe.map (divString "package-description") pj.description
      , kvDiv "Main" stringValue pj.main
      , kvDiv "Keywords" (listValue stringValue) pj.keywords
      , kvDiv "Repository" repoView pj.repository
      , kvDiv "Home Page" linkValue pj.homepage
      , kvDiv "Bugs" stringValue pj.bugs
      , kvDiv "License" stringValue pj.license
      , kvDiv "Author" personValue pj.author
      , kvDiv "Contributors" (listValue personValue) pj.contributors
      , kvDiv "Files" (listValue stringValue) pj.files
      , kvDiv "Man" (listValue stringValue) pj.man
      , kvDiv "Bin" pairValue pj.bin
      , kvDiv "scripts" (listValue pairValue) pj.scripts
      , kvDiv "config" (listValue pairValue) pj.config
      , kvDiv "Dependencies" (listValue depView) pj.dependencies
      , kvDiv "Dev Dependencies" (listValue depView) pj.devDependencies
      , kvDiv "Peer Dependencies" (listValue depView) pj.peerDependencies
      , kvDiv "Bundled Dependencies" (listValue stringValue) pj.bundledDependencies
      , kvDiv "Bundle Dependencies" (listValue stringValue) pj.bundleDependencies
      , kvDiv "Optional Dependencies" (listValue depView) pj.optionalDependencies
      , kvDiv "Engines" (listValue pairValue) pj.engines
      , kvDiv "OS" (listValue stringValue) pj.os
      , kvDiv "CPU" (listValue stringValue) pj.cpu
      , kvDiv "Prefer Global" (boolValue) pj.preferGlobal
      , kvDiv "Private" (boolValue) pj.private
      , kvDiv "Publish Config" (listValue pairValue) pj.publishConfig
      ]
  in
    div [ class "package-json" ]
      <| List.filterMap identity fields


spanString : String -> String -> Html
spanString className name =
    span [ class className ] [ text name ]

divString : String -> String -> Html
divString className name =
    div [ class className ] [ text name ]

stringValue : String -> Html
stringValue value =
  span [ class "field-value" ] [ text value ]

depView : (String, String) -> Html
depView (name, version) =
    let
        match typ arr =
            List.any ((==) typ) arr
        componentView (verType, val) =
            if match verType ["git", "http", "file"] then
                a [ href val ] [ text val ]
            else if verType == "major" then
                span [ ] [ text val ]
            else if verType == "tag" then
                span [ ] [ text ("'" ++ val ++ "'") ]
            else if verType == "minor" then
                span [ ] [ text ("." ++ val) ]
            else if verType == "patch" then
                span [ ] [ text ("." ++ val ++ " ") ]
            else if match verType ["or", "to"] then
                span [ class "semver-symbol" ] [ text (" " ++ verType ++ " (" ++ val ++ ") ") ]
            else if verType == "exactly" then
                span [ class "semver-symbol" ] [ text (verType ++ " ") ]
            else
                span [ class "semver-symbol" ] [ text (verType ++ " (" ++ val ++ ")") ]
        parsed =
            parseSemVer version
            |> List.map componentView
    in
        div [ class "dep-view" ]
            [ a [ class "dep-name", href ("https://www.npmjs.com/package/" ++ name) ] [ text name ]
            , span [ ] [ text ": " ]
            , span [ class "dep-version" ] parsed
            ]


listValue : (a -> Html) -> List a -> Html
listValue view values =
  let
    listItem value =
      li [] [ view value ]
  in
    ul [ class "sub-field" ]
      <| List.map listItem values


boolValue : Bool -> Html
boolValue bl =
  case bl of
    True ->
      stringValue "True"

    False ->
      stringValue "False"


listItemValue : String -> String -> Html
listItemValue key value =
  li [] [ fieldView key <| stringValue value ]


linkValue : String -> Html
linkValue url =
  span
    [ class "field-value" ]
    [ a [ href url ] [ text url ] ]


pairValue : ( String, String ) -> Html
pairValue ( name, filePath ) =
  fieldView name (stringValue filePath)


personValue : PackageJson.Person -> Html
personValue { name, email, url } =
  [ ( "Name", name ), ( "Email", email ), ( "Url", url ) ]
    |> List.filterMap (\( key, value ) -> Maybe.map (listItemValue key) value)
    |> ul [ class "sub-field" ]


directoriesValue : PackageJson.Directories -> Html
directoriesValue { bin, doc, lib, man, example, tests } =
  [ ( "Bin", bin ), ( "Doc", doc ), ( "Lib", lib ), ( "Man", man ), ( "Example", example ), ( "Tests", tests ) ]
    |> List.filterMap (\( key, value ) -> Maybe.map (listItemValue key) value)
    |> ul [ class "sub-field" ]


fieldView : String -> Html -> Html
fieldView key value =
  div
    [ class "field" ]
    [ span [ class "field-title" ] [ text (key ++ ": ") ]
    , value
    ]


linkFieldView : String -> String -> Html
linkFieldView key url =
  div
    [ class "field" ]
    [ span [ class "field-title" ] [ text (key ++ ": ") ]
    , span [ class "field-value" ] [ a [ href url ] [ text url ] ]
    ]


repoView : ( String, String ) -> Html
repoView ( repoType, url ) =
  ul
    [ class "sub-field" ]
    [ li [] [ fieldView "type" <| stringValue repoType ]
    , li [] [ fieldView "url" <| linkValue url ]
    ]



-- Utils


receiveFile : Json.Encode.Value -> Action
receiveFile value =
  Fs.decode value
    |> decodeContents


decodeContents : File -> Action
decodeContents file =
  case toLower (.name file) of
    "package.json" ->
      ReceivePackageJson (.contents file)

    _ ->
      FileError <| Just "File not recognized"


fetchFile : String -> Effects Action
fetchFile filePath =
  Signal.send fetchFileBox.address filePath
    |> Effects.task
    |> Effects.map (always NoOp)


fetchFileBox : Signal.Mailbox String
fetchFileBox =
  Signal.mailbox ""
