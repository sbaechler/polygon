
module Game where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Debug


-- MODEL
(gameWidth,gameHeight) = (800,600)
(halfWidth,halfHeight) = (400,300)



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
      if state == Play then
       score + 1
      else 
        score

    newAngle =  Debug.watch "Player angle" (updatePlayerAngle player.angle dir)


    newState =
      if space then
          Play

      else
          state

  in
    { game |
        state = newState,
        player = { player | angle = newAngle },
        score = newScore
    }


updatePlayerAngle: Float -> Int -> Float
updatePlayerAngle angle dir = 
  let 
    newAngle = (angle + (toFloat dir) * 0.05)
  in 
    if newAngle < 0 then
      newAngle + 2*pi
    else if newAngle > 2*pi then
      newAngle - 2*pi
    else
      newAngle

-- VIEW
bgBlack =
  rgb 40 40 40

uiColor =
  rgb 160 200 160

playerRadius = 
  100


txt f string =
  Text.fromString string
    |> Text.color uiColor
    |> Text.monospace
    |> f
    |> leftAligned


msg = "SPACE to start, &larr;&rarr; to move"

makePlayer player =
  ngon 3 10
    |> filled (hsl player.angle 1 0.5)
    |> move (playerRadius * cos player.angle, playerRadius * sin player.angle)
    |> rotate (player.angle)



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
      , makePlayer game.player
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
  Signal.map inSeconds (fps 60)


input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Keyboard.space
      (Signal.map .x Keyboard.arrows)
      delta