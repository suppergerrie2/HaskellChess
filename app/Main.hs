module Main where

import Graphics.Gloss
  ( Display (InWindow),
    translate,
    white, play,
  )
import Graphics.Gloss.Interface.IO.Interact (Event (EventMotion, EventKey), MouseButton (LeftButton, RightButton), Key (MouseButton), KeyState (Down, Up))
import Debug.Trace (trace)
import Data.Maybe (isNothing, isJust)
import GameState(GameState (mousePos, selectedLocation), initGameState)
import Drawing (loadImages, windowSize, windowWidth, windowHeight, drawGameState, findPieceAtPos, getTilePosFromScreenPos)

main :: IO ()
main = do
  images <- loadImages
  play
    ( InWindow
        "Chess Time" -- window title
        windowSize -- window size
        (10, 10) -- window position
    )
    white -- background color
    60
    (initGameState images)
    (translate (-(fromIntegral windowWidth / 2)) (-(fromIntegral windowHeight / 2)) . drawGameState)
    input
    updateWorld

updateWorld :: Float -> GameState -> GameState
updateWorld _ b = b

input :: Event -> GameState -> GameState
input (EventMotion (mx, my)) gs = gs {mousePos = (mx + fromIntegral windowWidth / 2, my + fromIntegral windowHeight / 2)}
input (EventKey (MouseButton LeftButton) Down _ _) gs | isNothing . selectedLocation $ gs = gs {selectedLocation = Just location}
  where
    location = getTilePosFromScreenPos (mousePos gs)
input (EventKey (MouseButton LeftButton) Up _ _) gs | isJust . selectedLocation $ gs = gs {selectedLocation = Nothing}
input e gs = trace (show e) gs