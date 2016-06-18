-- Currently not in use.

module Music exposing (hasBass, loadSound, playbackOptions)

import Audio exposing (defaultPlaybackOptions, Sound)
import Dict
import Task exposing (Task)


import Time exposing (Time)


bassSwitch : List Int -- ms
bassSwitch = [14760, 44313, 51668, 129193, 14387]

-- possible additional entry points
entryPoints = [84718]

-- point where to loop
loopPoint = 188331

hasBass : Time -> Bool
--hasBass time =
--  if time < 14760 then False
--  else if time < 44313 then True
--  else if time < 51668 then False
--  else if time < 129193 then True
--  else if time < 14387 then False
--  else True


hasBass time =
  if time < 20894 then False
  else if time < 41976 then True
  else if time < 55672 then False
  else if time < 67842 then True
  else if time < 187846 then False
  else if time < 215938 then True
  else False

loadSound : Task String Sound
loadSound = Audio.loadSound "music/music.mp3"


playbackOptions = {
  defaultPlaybackOptions | loop = True, startAt = Nothing }
