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
        fields =
            [ Maybe.map (fieldView "Name") pj.name
            , Maybe.map (fieldView "Version") pj.version
            , Maybe.map (fieldView "Description") pj.description
            , Maybe.map (fieldView "Main") pj.main
            , Maybe.map repoView pj.repository
            ]
    in
        div [ class "package-json" ]
            <| List.filterMap identity fields

fieldView : String -> String -> Html
fieldView key value =
    div [ class "field" ]
        [ span [ class "field-title" ] [ text (key ++ ": ") ]
        , span [ class "field-value" ] [ text value ]
        ]

linkFieldView : String -> String -> Html
linkFieldView key url =
    div [ class "field" ]
        [ span [ class "field-title" ] [ text (key ++ ": ") ]
        , span [ class "field-value" ] [ a [ href url ] [ text url ] ]
        ]

repoView : (String, String) -> Html
repoView (repoType, url)=
    div [ class "field" ]
    [ span [ class "field-title" ] [ text "Repository: " ]
    , ul [ class "sub-field" ]
        [ li [ ] [ fieldView "type" repoType ]
        , li [ ] [ linkFieldView "url" url ]
        ]
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
