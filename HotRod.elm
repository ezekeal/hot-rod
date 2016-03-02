module HotRod where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode
import String exposing (toLower)
import PackageJson exposing (PackageJson)
import Fs exposing (File)


-- MODEL

type alias Model =
    { packageJson: PackageJson }

init : (Model, Effects Action)
init =
    ( { packageJson = PackageJson.default }
    , Effects.none
    )


-- UPDATE

type Action
    = NoOp
    | RequestFile String
    | ReceivePackageJson String

update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        NoOp ->
            (model, Effects.none)

        RequestFile filePath ->
            (model, fetchFile filePath)

        ReceivePackageJson value ->
            ( { model | packageJson = (PackageJson.decode value) }
            , Effects.none
            )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    div [ ]
        [ div [ class "get-package-json" ]
            [ button
                [ onClick address (RequestFile "package.json") ]
                [ text "get package.json" ]
            ]
        , packageJsonView model.packageJson
        ]

packageJsonView : PackageJson -> Html
packageJsonView pj =
    let
        kvDiv key valueView value =
            Maybe.map (valueView >> fieldView key) value
        fields =
            [ kvDiv "Name" stringValue pj.name
            , kvDiv "Version" stringValue pj.version
            , kvDiv "Description" stringValue pj.description
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
            , kvDiv "Dependencies" (listValue pairValue) pj.dependencies
            , kvDiv "Dev Dependencies" (listValue pairValue) pj.devDependencies
            , kvDiv "Peer Dependencies" (listValue pairValue) pj.peerDependencies
            , kvDiv "Bundeled Dependencies" (listValue stringValue) pj.bundeledDependencies
            , kvDiv "Bundele Dependencies" (listValue stringValue) pj.bundeleDependencies
            , kvDiv "Optional Dependencies" (listValue pairValue) pj.optionalDependencies
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

stringValue : String -> Html
stringValue value =
    span [ class "field-value" ] [ text value ]

listValue : (a -> Html) -> List a -> Html
listValue view values =
    ul [ class "sub-field" ]
        <| List.map view values

boolValue: Bool -> Html
boolValue bl =
    case bl of
        True ->
            stringValue "True"
        False->
            stringValue "False"

listItemValue : String -> String -> Html
listItemValue key value =
    li [ ] [ fieldView key <| stringValue value ]

linkValue : String -> Html
linkValue url =
    span [ class "field-value" ]
        [ a [ href url ] [ text url ] ]

pairValue : (String,String) -> Html
pairValue (name, filePath) =
    fieldView name (stringValue filePath)

personValue : PackageJson.Person -> Html
personValue {name, email, url} =
    [ ("Name" , name), ("Email",email), ("Url",url) ]
    |> List.filterMap (\(key, value) -> Maybe.map (listItemValue key) value)
    |> ul [ class "sub-field" ]

directoriesValue : PackageJson.Directories -> Html
directoriesValue {bin, doc, lib, man, example, tests} =
    [ ("Bin",bin), ("Doc",doc), ("Lib",lib), ("Man",man), ("Example",example), ("Tests", tests) ]
    |> List.filterMap (\(key, value) -> Maybe.map (listItemValue key) value)
    |> ul [ class "sub-field" ]

fieldView : String -> Html -> Html
fieldView key value =
    div [ class "field" ]
        [ span [ class "field-title" ] [ text (key ++ ": ") ]
        , value
        ]

linkFieldView : String -> String -> Html
linkFieldView key url =
    div [ class "field" ]
        [ span [ class "field-title" ] [ text (key ++ ": ") ]
        , span [ class "field-value" ] [ a [ href url ] [ text url ] ]
        ]

repoView : (String, String) -> Html
repoView (repoType, url)=
    ul [ class "sub-field" ]
        [ li [ ] [ fieldView "type" <| stringValue repoType ]
        , li [ ] [ fieldView "url" <| linkValue url ]
        ]



-- Utils

receiveFile : Json.Encode.Value -> Action
receiveFile value =
    Fs.decode value
    |> decodeContents

decodeContents : File -> Action
decodeContents file =
    case toLower (.extension file) of
        ".json" ->
            if toLower (.name file) == "package.json" then
                ReceivePackageJson (.contents file)
            else NoOp

        _ -> NoOp

fetchFile : String -> Effects Action
fetchFile filePath =
    Signal.send fetchFileBox.address filePath
    |> Effects.task
    |> Effects.map (always NoOp)

fetchFileBox : Signal.Mailbox String
fetchFileBox =
    Signal.mailbox ""
