
import Game

import Task
import Effects
import Graphics.Element exposing ( Element )


main : Signal Graphics.Element.Element
main =
  Game.init

-- port tasks : Signal (Task.Task Effects.Never ())
-- port tasks =
--  app.tasks
