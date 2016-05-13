module Game

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
import String exposing (padLeft)
import Random


-- MODEL
(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)
radius : Float
radius = halfWidth * 1.42
obstacleThickness = 30


-- Type definitions
type State = NewGame | Play | Pause | GameOver

type alias Player =
  { angle: Float }

type alias Game =
  { state: State,
    player : Player,
    obstacles: List(Obstacle),
    progress : Int,
    obstacleSpeed: Float,
    autoRotateAngle: Float,
    autoRotateSpeed: Float
  }

type alias Obstacle =
  { radius: Float
  , parts: List(Bool)
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
  { state = NewGame
  , player = Player (degrees 30)
  , obstacles = []
  , progress = 0
  , obstacleSpeed = 0
  , autoRotateAngle = 0.0
  , autoRotateSpeed = 0.0
  }




-- UPDATE

-- Game loop: Transition from one state to the next.
update : Input -> Game -> Game
update input game =
  { game |
      state = updateState input game,
      player = updatePlayer input game,
      obstacles = updateObstacles game,
      progress = updateProgress game,
      obstacleSpeed = Debug.watch "obstacle speed" (1.5 + (toFloat game.progress)/1000) ,
      autoRotateAngle = updateAutoRotateAngle game,
      autoRotateSpeed = updateAutoRotateSpeed game
  }

colidesWith: Player -> Obstacle -> Bool
colidesWith player obstacle =
  let
    collidesAtIndex: Int -> Bool
    collidesAtIndex index =
      let
        fromAngle = Debug.watch ("from Angle"++toString index) ((toFloat index) * 60)

        toAngle = Debug.watch ("to Angle"++ toString index) (((toFloat index)+1)*60)
        playerDegrees = Debug.watch "player degrees" (player.angle * 360 / (2*pi))
      in
        playerDegrees >= fromAngle && playerDegrees < toAngle
  in
    if obstacle.radius > playerRadius || obstacle.radius + obstacleThickness < playerRadius then
      False
    else
      -- check if open

        indexedMap (,) obstacle.parts |> filter snd |> map fst |> any collidesAtIndex


isGameOver: Game -> Bool
isGameOver {player, obstacles} =
  any (colidesWith player) obstacles

updateState: Input -> Game -> State
updateState input game =
  case game.state of
    NewGame -> if input.space then Play else Pause
    Play ->
      if input.space then Pause else
        if isGameOver game then GameOver else Play
    Pause -> if input.space then Play else Pause
    GameOver -> if input.space then NewGame else GameOver

updatePlayer: Input -> Game -> Player
updatePlayer {dir} {player, state} =
  if state == Play then
    let
      newAngle = if state == NewGame then degrees 30 else
        Debug.watch "Player angle" (updatePlayerAngle player.angle -dir)
    in
      { player | angle = newAngle }
  else
    player

updateObstacles: Game -> List(Obstacle)
updateObstacles game =
  let
    obstacleDistance = 300
    partsFor index =
      case index of
        0 -> [True, True, True, False, True, True]
        1 -> [True, True, True, False, True, True]
        2 -> [False, True, False, True, True, True]
        3 -> [False, True, True, True, True, True]
        _ -> [True, False, True, True, True, True]
    radiusFor index =
      Debug.watch ("obstacle radius"++toString index)
      toFloat (obstacleThickness + (iHalfWidth + round (( obstacleDistance * (toFloat index)) - (toFloat game.progress) * game.obstacleSpeed)) % (obstacleDistance * 5))
  in
   [
      {parts = partsFor 0, radius = radiusFor 0}
    , {parts = partsFor 1, radius = radiusFor 1}
    , {parts = partsFor 2, radius = radiusFor 2}
    , {parts = partsFor 3, radius = radiusFor 3}
    , {parts = partsFor 4, radius = radiusFor 4}
    ]


updateProgress: Game -> Int
updateProgress {state,progress} =
  case state of
    NewGame -> 0
    Play -> progress + 1
    Pause -> progress
    GameOver -> progress

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
    newAngle = (angle + toFloat (dir * 4) * 0.032)
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
  let
    angle = player.angle - degrees 30
  in
    ngon 3 10
      |> filled (hsl angle 1 0.5)
      |> moveRadial angle (playerRadius - 10)
      |> rotate angle




trapezoid: Float -> Float -> Color -> Form
trapezoid base height color =
  let
    s = height/(tan <| degrees 60)
  in
    filled color <| polygon [
      (-base/2, 0), (base/2, 0), (base/2-s, height), (-base/2+s, height)
    ]



makeObstacle : Color -> Obstacle -> Form
makeObstacle color obstacle =
  let
    base = 2.0 * (obstacle.radius +obstacleThickness) / (sqrt 3)
    makeObstaclePart : Int -> Form
    makeObstaclePart index =
      trapezoid base obstacleThickness color
        |> rotate (degrees <| toFloat (90 + index * 60))
        |> moveRadial (degrees <| toFloat (index * 60)) (obstacle.radius +obstacleThickness)

    -- color = (hsl (radius/100) 1 0.5)
  in
    group
      (indexedMap (,) obstacle.parts |> filter snd |> map fst |> map makeObstaclePart)

makeObstacles : Color -> List(Obstacle) -> List(Form)
makeObstacles color obstacles =
  map (makeObstacle color) obstacles



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

-- Create a clock that shows 1/100 seconds
formatScore : Game -> String
formatScore {state, progress} =
  let
    time = progress * 100 // 60
    seconds = time // 100
    centis = time % 100
  in
    padLeft 3 '0' (toString seconds) ++ "." ++ padLeft 2 '0' (toString centis)


view : (Int,Int) -> Game -> Element
view (w, h) game =
  let
    score =
      formatScore game
      |> txt (Text.height 50)
    colors = makeColors game.progress
    message = txt (Text.height 50) <|
      case game.state of
        GameOver -> "Game Over"
        Pause -> "Pause"
        _ -> ""

  in
    container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled bgBlack
      , group (append
        [ makeField colors
        , group <| makeObstacles colors.bright game.obstacles
        , makePlayer game.player
        ]
        (makeCenterHole colors game.progress)
      )
      |> rotate game.autoRotateAngle
      , toForm score
          |> move (100 - halfWidth, halfHeight - 40)
      , toForm message
          |> move (0, 40)
      , toForm (if game.state == Play then spacer 1 1 else txt identity msg)
          |> move (0, 40 - halfHeight)
      ]


-- SIGNALS

main : Signal Element
main =
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
