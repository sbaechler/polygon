
module Game where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Debug


-- MODEL
(gameWidth,gameHeight) = (800,600)
(halfWidth,halfHeight) = (400,300)
radius = halfWidth * 1.42


-- Type definitions
type State = Play | Pause

type alias Player =
  { angle: Float }

type alias Game =
  { state: State,
    player : Player,
    progress : Int
  }

type alias Input =
  { space : Bool
  , dir : Int
  , delta : Time
  }


-- The global game state
defaultGame : Game
defaultGame =
  { state = Pause
  , player = Player 0.0
  , progress = 0
  }

-- UPDATE

-- Game loop: Transition from one state to the next.
update : Input -> Game -> Game
update {space,dir,delta} ({state,player,progress} as game) =
  let
    newProgress =
      if state == Play then
        progress + 1
      else 
        progress

    newAngle =  Debug.watch "Player angle" (updatePlayerAngle player.angle -dir)

    newState =
      if space then
        Play
      else
        state

  in
    { game |
        state = newState,
        player = { player | angle = newAngle },
        progress = newProgress
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

moveRadial angle radius = 
  move (radius * cos angle, radius * sin angle)
makePlayer player =
  ngon 3 10
    |> filled (hsl player.angle 1 0.5)
    |> moveRadial player.angle (playerRadius - 10)
    |> rotate (player.angle)


obstacleThickness = 20

trapez: Float -> Float -> Color -> Form
trapez base height color= 
  let 
    s = height/(tan (degrees 60))
  in
    filled color (polygon [
      (-base/2,0),(base/2,0),(base/2-s,height),(-base/2+s,height)
    ])
makeObstacle radius opening = 
  let 
    base = 2.0* radius / (sqrt 3)
    color = (hsl (radius/100) 1 0.5)
  in
    group 
      [ (trapez base obstacleThickness color) |> rotate (degrees 90) |> moveRadial (degrees 0) radius
      , (trapez base obstacleThickness color) |> rotate (degrees 150) |> moveRadial (degrees 60) radius
      , (trapez base obstacleThickness color) |> rotate (degrees 210) |> moveRadial (degrees 120) radius
      , (trapez base obstacleThickness color) |> rotate (degrees 270) |> moveRadial (degrees 180) radius
      , (trapez base obstacleThickness color) |> rotate (degrees 330) |> moveRadial (degrees 240) radius
      --, (trapez base 20) |> rotate (degrees 30) |> moveRadial (degrees 300) radius
      ] |> rotate (degrees opening * 60)


makeObstacles progress = 
  let 
    radius1 = Debug.watch "obstacleradius" (obstacleThickness + toFloat ((halfWidth - progress) % halfWidth))
    radius2 = Debug.watch "obstacleradius" (obstacleThickness + toFloat ((100 + halfWidth - progress) % halfWidth))
    radius3 = Debug.watch "obstacleradius" (obstacleThickness + toFloat ((200 + halfWidth - progress) % halfWidth))
  in
    group 
    [ makeObstacle radius1 0
    , makeObstacle radius2 1
    , makeObstacle radius3 2
    ]


hexagonElement: Float -> Int -> List((Float, Float))
hexagonElement r i =
  Debug.watch "Element" [(0.0, 0.0),
   (sin (degrees (toFloat (60 * i))) * r,
    cos (degrees (toFloat (60 * i))) * r),
   (sin (degrees (toFloat (60 * (i+1)))) * r,
    cos (degrees (toFloat (60 * (i+1)))) * r)
  ]


makeField: Float -> Form
makeField hue =
  let
    darkColor = hsl hue 0.7 0.2
    brightColor = hsl hue 0.7 0.5

    color i =
      if i%2 == 0 then
        darkColor
      else
        brightColor

    poly i =
      polygon (hexagonElement radius i)
      |> filled (color i)

  in
    group (map poly [0..5])



-- Render the game to the DOM.
view : (Int,Int) -> Game -> Element
view (w, h) game =
  let
    progress =
      txt (Text.height 50) (toString game.progress)
  in
    container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled bgBlack
      , makeField 1.1
      , makeObstacles game.progress
      , makePlayer game.player
      , toForm progress
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

-- Returns a clock signal
delta =
  Signal.map inSeconds (fps 60)

-- Creates an event stream from the keyboard inputs and the
-- clock.
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Keyboard.space
      (Signal.map .x Keyboard.arrows)
      delta