
import Game

import StartApp
import Task
import Effects
import Html

app : StartApp.App Game.Model
app =
  StartApp.start
    { view = Game.view
    , update = Game.update
    , init = Game.init
    , inputs = []
    }

main : Signal Html.Html
main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
