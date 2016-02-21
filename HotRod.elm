module HotRod where

import Effects exposing (Effects, Never)
import Json.Encode exposing (Value)
import Graphics.Element exposing(show)
import Html exposing (..)
import Html.Events exposing (onClick)

-- MODEL

type alias Model = Value

init : (Model, Effects Action)
init =
  ( Json.Encode.object [ ]
  , Effects.none
  )


-- UPDATE

type Action
  = NoOp
  | RequestPackageJson
  | ReceivePackageJson Value

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      (model, Effects.none)

    RequestPackageJson ->
      (model, fetchPackageJson)

    ReceivePackageJson value ->
      (value, Effects.none)

fetchPackageJson : Effects Action
fetchPackageJson =
  Signal.send fetchPackageJsonBox.address ()
  |> Effects.task
  |> Effects.map (always NoOp)

fetchPackageJsonBox : Signal.Mailbox ()
fetchPackageJsonBox = Signal.mailbox ()

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ ]
    [ button  [ onClick address RequestPackageJson ] [ text "click me" ]
    , fromElement (show model)
    ]
