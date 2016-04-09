
module Game where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window

-- MODEL
(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)


type State = Play | Pause

type alias Player =
  { angle: Float }

type alias Game =
  { state: State,
    player : Player,
    score : Int
  }

type alias Input =
  { space : Bool
  , dir : Int
  , delta : Time
  }


defaultGame : Game
defaultGame =
  { state = Pause
  , player = Player 0.0
  , score = 0
  }

-- UPDATE

update : Input -> Game -> Game
update {space,dir,delta} ({state,player,score} as game) =
  let
    newScore =
      score + 1

    newState =
      if space then
          Play

      else
          state

  in
    { game |
        state = newState,
        player = player,
        score = newScore
    }



-- VIEW
bgBlack =
  rgb 40 40 40

uiColor =
  rgb 160 200 160


txt f string =
  Text.fromString string
    |> Text.color uiColor
    |> Text.monospace
    |> f
    |> leftAligned


msg = "SPACE to start, &uarr;&darr; to move"

make obj shape =
  shape
    |> filled white
    |> move (0, 0)



view : (Int,Int) -> Game -> Element
view (w, h) game =
  let
    score =
      txt (Text.height 50) (toString game.score)
  in
    container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled bgBlack
      , oval 15 15
          |> make game.player
      , toForm score
          |> move (0, gameHeight/2 - 40)
      , toForm (if game.state == Play then spacer 1 1 else txt identity msg)
          |> move (0, 40 - gameHeight/2)
      ]



-- SIGNALS

main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input


delta =
  Signal.map inSeconds (fps 30)


input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Keyboard.space
      (Signal.map .x Keyboard.arrows)
      delta