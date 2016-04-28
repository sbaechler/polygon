module Game where

import AnimationFrame
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Keyboard
import Text
import Time exposing ( .. )
import Window
import Debug


-- MODEL
(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)
radius : Float
radius = halfWidth * 1.42
obstacleThickness = 30


-- Type definitions
type State = Play | Pause

type alias Player =
  { angle: Float }

type alias Game =
  { state: State,
    player : Player,
    progress : Int,
    autoRotateAngle: Float,
    autoRotateSpeed: Float
  }

type alias Input =
  { space : Bool
  , dir : Int
  , delta : Time.Time
  }

type alias Colors =
  { dark : Color
  , medium: Color
  , bright : Color
  }


-- The global game state
defaultGame : Game
defaultGame =
  { state = Pause
  , player = Player 0.0
  , progress = 0
  , autoRotateAngle = 0.0
  , autoRotateSpeed = 0.0
  }

-- UPDATE

-- Game loop: Transition from one state to the next.
update : Input -> Game -> Game
update input game =
  { game |
      state = if input.space then Play else game.state,
      player = updatePlayer input game,
      progress = updateProgress game,
      autoRotateAngle = updateAutoRotateAngle game,
      autoRotateSpeed = updateAutoRotateSpeed game
  }


updatePlayer: Input -> Game -> Player
updatePlayer {dir} {player} =
  let
    newAngle =  Debug.watch "Player angle" (updatePlayerAngle player.angle -dir)
  in
    { player | angle = newAngle }

updateProgress: Game -> Int
updateProgress {state,progress} =
  if state == Play then
    progress + 1
  else
    progress


updateAutoRotateAngle: Game -> Float
updateAutoRotateAngle {autoRotateAngle, autoRotateSpeed} =
  autoRotateAngle + autoRotateSpeed

updateAutoRotateSpeed: Game -> Float
updateAutoRotateSpeed {progress, autoRotateSpeed} =
  0.02 * sin (toFloat progress * 0.005 |> Debug.watch "φ")
  |> Debug.watch "autoRotateSpeed"


updatePlayerAngle: Float -> Int -> Float
updatePlayerAngle angle dir =
  let
    newAngle = (angle + toFloat (dir * speed) * 0.032)
  in
    if newAngle < 0 then
      newAngle + 2 * pi
    else if newAngle > 2 * pi then
      newAngle - 2 * pi
    else
      newAngle



-- VIEW
bgBlack : Color
bgBlack =
  rgb 20 20 20

uiColor : Color
uiColor =
  rgb 255 255 255

playerRadius : Float
playerRadius = gameWidth / 10.0

speed : Int
speed = 4

msg : String
msg = "SPACE to start, &larr;&rarr; to move"

txt : (Text.Text -> Text.Text) -> String -> Element
txt f string =
  Text.fromString string
    |> Text.color uiColor
    |> Text.monospace
    |> f
    |> leftAligned


moveRadial : Float -> Float -> Form -> Form
moveRadial angle radius =
  move (radius * cos angle, radius * sin angle)

makePlayer : Player -> Form
makePlayer player =
  ngon 3 10
    |> filled (hsl player.angle 1 0.5)
    |> moveRadial player.angle (playerRadius - 10)
    |> rotate player.angle



trapezoid: Float -> Float -> Color -> Form
trapezoid base height color =
  let
    s = height/(tan <| degrees 60)
  in
    filled color <| polygon [
      (-base/2, 0), (base/2, 0), (base/2-s, height), (-base/2+s, height)
    ]

makeObstacle : Float -> Color -> Float -> Form
makeObstacle radius color opening =
  let
    base = 2.0 * radius / (sqrt 3)

    -- color = (hsl (radius/100) 1 0.5)
  in
    group
      [ (trapezoid base obstacleThickness color) |> rotate (degrees 90) |> moveRadial (degrees 0) radius
      , (trapezoid base obstacleThickness color) |> rotate (degrees 150) |> moveRadial (degrees 60) radius
      , (trapezoid base obstacleThickness color) |> rotate (degrees 210) |> moveRadial (degrees 120) radius
      , (trapezoid base obstacleThickness color) |> rotate (degrees 270) |> moveRadial (degrees 180) radius
      , (trapezoid base obstacleThickness color) |> rotate (degrees 330) |> moveRadial (degrees 240) radius
      --, (trapezoid base 20) |> rotate (degrees 30) |> moveRadial (degrees 300) radius
      ] |> rotate (degrees opening * 60)

makeObstacles : Color -> Int -> Form
makeObstacles color progress =
  let
    radius1 = Debug.watch "obstacleradius" (obstacleThickness + toFloat ((iHalfWidth - progress * speed) % iHalfWidth))
    radius2 = Debug.watch "obstacleradius2" (obstacleThickness + toFloat ((iHalfWidth + 150 - progress * speed) % iHalfWidth))
    radius3 = Debug.watch "obstacleradius3" (obstacleThickness + toFloat ((iHalfWidth + 300 - progress * speed) % iHalfWidth))
  in
    group
    [ makeObstacle radius1 color 0
    , makeObstacle radius2 color 1
    , makeObstacle radius3 color 2
    ]

hexagonElement: Float -> Int -> List((Float, Float))
hexagonElement r i =
  let
    angle0 = 60 * i |> toFloat |> degrees
    angle1 = 60 * (i+1) |> toFloat |> degrees
  in
    [(0.0, 0.0)
    , (sin angle0 * r, cos angle0 * r)
    , (sin angle1 * r, cos angle1 * r)
    ]

makeField: Colors -> Form
makeField colors =
  let
    color i =
      if i % 2 == 0 then
        colors.dark
      else
        colors.medium

    poly i =
      polygon (hexagonElement radius i)
      |> filled (color i)

  in
    group (map poly [0..5])

-- the polygon in the center: this is just decoration, so it has no own state
makeCenterHole : Colors -> Int -> List Form
makeCenterHole colors progress =
  let
    shape = ngon 6 (60 + 10 * (0.2 * toFloat progress |> sin))
    line = solid colors.bright
  in
    [shape
      |> filled colors.dark
      |> rotate (degrees 90)
    ,shape
      |> (outlined {line | width = 4.0})
      |> rotate (degrees 90)
    ]

makeColors : Int -> Colors
makeColors progress =
  let
    hue = degrees 0.1 * (toFloat <| progress % 3600)
  in
    { dark = (hsl hue 0.6 0.2)
    , medium = (hsl hue 0.6 0.3)
    , bright = (hsla hue 0.6 0.6 0.8)
    }


-- Render the game to the DOM.
view : (Int,Int) -> Game -> Element
view (w, h) game =
  let
    progress =
      toString game.progress
      |> txt (Text.height 50)
    colors = makeColors game.progress

  in
    container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled bgBlack
      , group (append
        [ makeField colors
        , makeObstacles colors.bright game.progress
        , makePlayer game.player
        ]
        (makeCenterHole colors game.progress)
      )
      |> rotate game.autoRotateAngle
      , toForm progress
          |> move (60 - halfWidth, halfHeight - 40)
      , toForm (if game.state == Play then spacer 1 1 else txt identity msg)
          |> move (0, 40 - halfHeight)
      ]


-- SIGNALS
init: Signal Element
init =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

-- Returns a clock signal
delta =
  AnimationFrame.frame

-- Creates an event stream from the keyboard inputs and the
-- clock.
input : Signal Input
input =
  Signal.map3 Input
    Keyboard.space
    (Signal.map .x Keyboard.arrows)
    delta
  -- only update on a new frame
  |> Signal.sampleOn delta
