module HotRod where

import Effects exposing (Effects, Never)
import Graphics.Element exposing(show)
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
        , div [ ] [ fromElement (show model) ]
        ]

packageJsonView : PackageJson -> Html
packageJsonView pj =
    let
        fieldDiv key valueView value =
            Maybe.map (valueView >> fieldView key) value
        fields =
            [ fieldDiv "Name" stringValue pj.name
            , fieldDiv "Version" stringValue pj.version
            , fieldDiv "Description" stringValue pj.description
            , fieldDiv "Main" stringValue pj.main
            , fieldDiv "Keywords" listValue pj.keywords
            , fieldDiv "Repository" repoView pj.repository
            , fieldDiv "Home Page" linkValue pj.homepage
            , fieldDiv "Bugs" stringValue pj.bugs
            , fieldDiv "License" stringValue pj.license
            , fieldDiv "Author" personValue pj.author
            , fieldDiv "Contributors" (List.map personValue >> div [ ]) pj.contributors
            , fieldDiv "Files" listValue pj.files
            , fieldDiv "Man" listValue pj.man
            , fieldDiv "Bin" pairValue pj.bin
            ]
    in
        div [ class "package-json" ]
            <| List.filterMap identity fields

stringValue : String -> Html
stringValue value =
    span [ class "field-value" ] [ text value ]

listValue : List String -> Html
listValue values =
    let item str =
        li [ ] [ text str ]
    in
        ul  [ class "sub-field" ]
            <| List.map item values

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
