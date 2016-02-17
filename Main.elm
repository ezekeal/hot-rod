--import HotRod exposing (update, view)
-- import StartApp exposing (start)
-- import Effects exposing (Never)
-- import Task
import Html exposing (..)
import Json.Encode exposing (Value)
import Graphics.Element exposing (show)


-- app =
--   start
--     { init = init
--     , update = update
--     , view = view
--     , inputs = [ ]
--     }

main : Signal Html
main =
  Signal.map view packageJSON

view : Value -> Html
view data =
  div [ ]
  [ fromElement (show data) ]

  --app

-- init =
--   ({ message = "Subscribe to the `request` port in javascript to see this in action!" }
--   , Effects.none
--   )
--
-- view address model =
--   show "hello"
--   --show packageJSON
--
-- type Action
--   = NoOp
--
-- update action model =
--   case action of
--     NoOp ->
--       (model, Effects.none)
--
-- -- PORTS
--
-- port tasks : Signal (Task.Task Never ())
-- port tasks =
--     app.tasks

port packageJSON : Signal Value
