module HotRod where

import Effects exposing (Effects, Never)
import Graphics.Element exposing(show)
import Html exposing (..)
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
    | RequestPackageJson
    | ReceivePackageJson String

update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        NoOp ->
            (model, Effects.none)

        RequestFile filePath ->
            (model, fetchFile filePath)

        RequestPackageJson ->
            (model, fetchFile "package.json")

        ReceivePackageJson value ->
            ( { model | packageJson = (PackageJson.decode value) }
            , Effects.none
            )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    div [ ]
        [ button  [ onClick address RequestPackageJson ] [ text "click me" ]
        , div [ ] [ fromElement (show model) ]
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
