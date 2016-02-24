import HotRod exposing (update, view, init, fetchFileBox, receiveFile)
import Html exposing (Html)
import StartApp
import Effects
import Task
import Json.Encode

app : StartApp.App HotRod.Model
app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs =
            [ Signal.map receiveFile file ]
        }

main : Signal Html.Html
main =
  app.html


-- PORTS

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks

port fetchFile : Signal String
port fetchFile =
    fetchFileBox.signal

port file : Signal Json.Encode.Value
