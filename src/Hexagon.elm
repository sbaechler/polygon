import Time exposing (..)
import List exposing (..)
import AnimationFrame
import Keyboard.Extra as Keyboard
import Window
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Debug
import Text
import Html
import Html.App as App
import String exposing (padLeft)
import Task exposing (andThen)
import Audio exposing (Sound)

import Html
import Html.App as App
import Window exposing (Size)
import AnimationFrame

import Music exposing (..)


-- MODEL
type State = Loading | NewGame | Starting | Play | Pause | GameOver


type Msg
  = Step Time
  | KeyboardExtraMsg Keyboard.Msg
  | MusicLoaded Sound

type alias Player =
  { angle: Float }

type alias Enemy =
  { radius : Float
  , parts : List(Bool)
  }


type alias Game =
  {
    player : Player
  , enemies: List(Enemy)
  , enemySpeed: Float
  , state : State
  , progress : Int
  , timeStart : Time
  , timeTick : Time
  , msRunning : Float
  , autoRotateAngle : Float
  , autoRotateSpeed : Float
  , direction : Int
  , keyboardModel : Keyboard.Model
  , hasBass : Bool
  }

type alias Colors =
  { dark : Color
  , medium: Color
  , bright : Color
  }

(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)

bgBlack : Color
bgBlack =
  rgb 20 20 20

playerRadius : Float
playerRadius = gameWidth / 10.0

playerSpeed : Float
playerSpeed = 0.128

enemyThickness = 30

startMessage = "SPACE to start, &larr;&rarr; to move"

beat = 130.0 |> bpm
beatAmplitude = 0.06
beatPhase = 180 |> degrees

-- Calculate Beat Per Minute
bpm : Float -> Float
bpm beat =
  (2.0 * pi * beat / 3600 )

pump : Int -> Float
pump progress = beatAmplitude * (beat * toFloat progress + beatPhase |> sin)

--handleAudio : Game -> Audio.Action
--handleAudio game =
--  case game.state of
--    Play -> Audio.Play
--    Starting -> Audio.Seek 0
--    _ -> Audio.Pause
--
--propertiesHandler : Audio.Properties -> Maybe Audio.Action
--propertiesHandler properties = Nothing
--
--music : Signal (Audio.Event, Audio.Properties)
--music = Audio.audio { src = "music/music.mp3",
--                      triggers = Audio.defaultTriggers,
--                      propertiesHandler = propertiesHandler,
--                      actions = Signal.map handleAudio gameState }


-- UPDATE

updatePlayerAngle: Float -> Int -> Float
updatePlayerAngle angle dir =
  let
    newAngle = (angle + toFloat -dir * playerSpeed)
  in
    if newAngle < 0 then
      newAngle + 2 * pi
    else if newAngle > 2 * pi then
      newAngle - 2 * pi
    else
      newAngle

colidesWith: Player -> Enemy -> Bool
colidesWith player enemy =
  let
    collidesAtIndex: Int -> Bool
    collidesAtIndex index =
      let
        fromAngle = (toFloat index) * 60
        toAngle = ((toFloat index) + 1) * 60
        playerDegrees =  player.angle * 360 / (2*pi)
      in
        playerDegrees >= fromAngle && playerDegrees < toAngle
  in
    if enemy.radius > playerRadius || enemy.radius + enemyThickness < playerRadius then
      False
    else
      -- check if open
        indexedMap (,) enemy.parts |> filter snd |> map fst |> any collidesAtIndex

isGameOver: Game -> Bool
isGameOver {player, enemies} =
  any (colidesWith player) enemies


updateProgress: Game -> Int
updateProgress {state,progress} =
  case state of
    NewGame -> 0
    Play -> progress + 1
    _ -> progress

updateMsRunning: Time -> Game -> Time
updateMsRunning timestamp game =
  case game.state of
    Play -> game.msRunning + timestamp - game.timeTick
    NewGame -> 0.0
    _ -> game.msRunning

updateAutoRotateAngle: Game -> Float
updateAutoRotateAngle {autoRotateAngle, autoRotateSpeed} =
  autoRotateAngle + autoRotateSpeed

updateAutoRotateSpeed: Game -> Float
updateAutoRotateSpeed {progress, autoRotateSpeed} =
  0.02 * sin (toFloat progress * 0.005)

updatePlayer: Int -> Game -> Player
updatePlayer dir {player, state} =
  if state == Play then
    let
      newAngle = if state == NewGame then degrees 30
                 else updatePlayerAngle player.angle dir
    in
      { player | angle = newAngle }
  else
    player

updateEnemies: Game -> List(Enemy)
updateEnemies game =
  let
    enemyDistance = 300
    partsFor index =
      case index of
        0 -> [True, True, True, False, True, True]
        1 -> [True, True, True, False, True, True]
        2 -> [False, True, False, True, True, True]
        3 -> [False, True, True, True, True, True]
        _ -> [True, False, True, True, True, True]
    radiusFor index =
      toFloat (enemyThickness + (iHalfWidth + round (( enemyDistance * (toFloat index))
      - (toFloat game.progress) * game.enemySpeed)) % (enemyDistance * 5))
  in
   [
      {parts = partsFor 0, radius = radiusFor 0}
    , {parts = partsFor 1, radius = radiusFor 1}
    , {parts = partsFor 2, radius = radiusFor 2}
    , {parts = partsFor 3, radius = radiusFor 3}
    , {parts = partsFor 4, radius = radiusFor 4}
    ]

updateEnemySpeed: Game -> Float
updateEnemySpeed game =
  2 + (toFloat game.progress)/1000


-- Game loop: Transition from one state to the next.
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    KeyboardExtraMsg keyMsg ->
      let
        ( keyboardModel, keyboardCmd ) =
          Keyboard.update keyMsg game.keyboardModel
        spacebar = Keyboard.isPressed Keyboard.Space keyboardModel &&
                not (Keyboard.isPressed Keyboard.Space game.keyboardModel)
        nextState =
          case game.state of
            NewGame -> if spacebar then Starting else Pause
            Play -> if spacebar then Pause else Play
            Pause -> if spacebar then Play else Pause
            GameOver -> if spacebar then NewGame else GameOver
            _ -> game.state
      in
        ( { game | keyboardModel = keyboardModel
                 , direction = (Keyboard.arrows keyboardModel).x
                 , state = nextState
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd )
    Step time ->
      let
        nextState =
          case game.state of
            Starting -> Play
            Play -> if isGameOver game then GameOver else Play
            _ -> game.state
      in
        ( { game |
            player = updatePlayer game.direction game
          , enemies = updateEnemies game
          , enemySpeed = updateEnemySpeed game
          , state =  nextState
          , progress = updateProgress game
          , timeStart = if game.state == NewGame then time else game.timeStart
          , timeTick = time
          , msRunning = updateMsRunning time game
          , autoRotateAngle = updateAutoRotateAngle game
          , autoRotateSpeed = updateAutoRotateSpeed game
          , hasBass = Music.hasBass game.msRunning
        }, Cmd.none)
    MusicLoaded music ->
        ( { game | state = NewGame }
        , Cmd.none)

-- VIEW

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


makeEnemy : Color -> Enemy -> Form
makeEnemy color enemy =
  let
    base = 2.0 * (enemy.radius +enemyThickness) / (sqrt 3)
    makeEnemyPart : Int -> Form
    makeEnemyPart index =
      trapezoid base enemyThickness color
        |> rotate (degrees <| toFloat (90 + index * 60))
        |> moveRadial (degrees <| toFloat (index * 60)) (enemy.radius +enemyThickness)

  in
    group
      (indexedMap (,) enemy.parts |> filter snd |> map fst |> map makeEnemyPart)

makeEnemies : Color -> List(Enemy) -> List(Form)
makeEnemies color enemys =
  map (makeEnemy color) enemys


hexagonElement: Int -> List((Float, Float))
hexagonElement i =
  let
    radius = halfWidth * sqrt 2
    angle0 = 60 * i |> toFloat |> degrees
    angle1 = 60 * (i+1) |> toFloat |> degrees
  in
    [(0.0, 0.0)
    , (sin angle0 * radius, cos angle0 * radius)
    , (sin angle1 * radius, cos angle1 * radius)
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
      polygon (hexagonElement i)
      |> filled (color i)
  in
    group (map poly [0..5])

-- the polygon in the center: this is just decoration, so it has no own state
makeCenterHole : Colors -> Game -> List Form
makeCenterHole colors game =
  let
    bassAdd = if game.hasBass then
        100.0 * beatAmplitude
      else
        100.0 * (pump game.progress)
    shape = ngon 6 (60 + bassAdd)
    line = solid colors.bright
  in
    [ shape
        |> filled colors.dark
        |> rotate (degrees 90)
    , shape
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

makeTextBox : Float -> String -> Element
makeTextBox size string =
  Text.fromString string
    |> Text.color (rgb 255 255 255)
    |> Text.monospace
    |> Text.height size
    |> leftAligned

beatPulse : Game -> Form -> Form
beatPulse game =
  if game.hasBass then
    scale (1 + (pump game.progress))
  else
    identity

formatTime : Time -> String
formatTime running =
  let
    centiseconds = floor (Time.inMilliseconds running / 10)
    seconds = centiseconds // 100
    centis = centiseconds % 100
  in
    padLeft 3 '0' (toString seconds) ++ "." ++ padLeft 2 '0' (toString centis)


view : Game -> Html.Html Msg
view game =
  let
    colors = makeColors game.progress
    score =
      formatTime game.msRunning
      |> makeTextBox 50
    message = makeTextBox 50 <|
      case game.state of
        GameOver -> "Game Over"
        Pause -> "Pause"
        _ -> ""
    bg = rect gameWidth gameHeight |> filled bgBlack
    field = append
            [ makeField colors
            , makePlayer game.player
            , group <| makeEnemies colors.bright game.enemies
            ]
            (makeCenterHole colors game)
          |> group

  in
    toHtml <|
    container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
      [ bg
      , field |> rotate game.autoRotateAngle |> beatPulse game
      , toForm message |> move (0, 40)
      , toForm score |> move (100 - halfWidth, halfHeight - 40)
      , toForm (
          if game.state == Play then spacer 1 1
          else makeTextBox 20 startMessage
        ) |> move (0, 40 - halfHeight)
    ]


-- SUBSCRIPTIONS


subscriptions : Game -> Sub Msg
subscriptions game =
  Sub.batch [
    AnimationFrame.times (\time -> Step time),
    Sub.map KeyboardExtraMsg Keyboard.subscriptions
  ]


--INIT

init : (Game, Cmd Msg)
init =
  let
    ( keyboardModel, keyboardCmd ) = Keyboard.init
  in
    ( { player = Player (degrees 30)
      , enemies = []
      , enemySpeed = 0.0
      , state = Loading
      , progress = 0
      , timeStart = 0.0
      , timeTick = 0.0
      , msRunning = 0.0
      , autoRotateAngle = 0.0
      , autoRotateSpeed = 0.0
      , hasBass = False
      , keyboardModel = keyboardModel
      , direction = 0
      }
    , Cmd.batch
      [ Cmd.map KeyboardExtraMsg keyboardCmd
      , Task.perform (\_ -> MusicLoaded Audio.Sound) (\music -> MusicLoaded music) loadSound
      ]
    )


main =
  App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions }
