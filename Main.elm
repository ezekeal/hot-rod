import HotRod exposing (update, view, init, Action, fetchPackageJsonBox)
import Html exposing (Html)
import StartApp exposing (start)
import Effects exposing (Never)
import Task
import Json.Encode exposing (Value)

app : StartApp.App HotRod.Model
app =
  start
    { init = init
    , update = update
    , view = view
    , inputs =
      [ Signal.map HotRod.ReceivePackageJson packageJson ]
    }

main : Signal Html.Html
main =
  app.html


-- PORTS

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks

port packageJson : Signal Value

port fetchPackageJsonSignal : Signal ()
port fetchPackageJsonSignal =
  fetchPackageJsonBox.signal
