module Main where

import Data.Maybe (isJust, isNothing)
import Debug.Trace (trace)
import Drawing (drawGameState, getTilePosFromScreenPos, loadImages, windowHeight, windowSize, windowWidth)
import GamePlay (attemptMove, updateWorld, updateFrequency, foldDirection, forDirection)
import GameState (GameState (currentState, mousePos, selectedLocation, GameState), State (..), initGameState, PlayerType (Player), BoardPiece)
import Graphics.Gloss
  ( Display (InWindow),
    play,
    translate,
    white, Picture,
  )
import Graphics.Gloss.Interface.IO.Interact (Event (EventKey, EventMotion), Key (MouseButton), KeyState (Down, Up), MouseButton (LeftButton))
import Menu (updateMenuInput, drawMenu)

main :: IO ()
main = do
  putStrLn "Starting Chess Time"
  images <- loadImages
  putStrLn "Images loaded"
  play
    ( InWindow
        "Chess Time" -- window title
        windowSize -- window size
        (10, 10) -- window position
    )
    white -- background color
    updateFrequency
    (initGameState images Player Player)
    (translate (-(fromIntegral windowWidth / 2)) (-(fromIntegral windowHeight / 2)) . draw)
    input
    updateWorld

draw :: GameState -> Picture
draw gs = case currentState gs of 
            MainMenu _ -> drawMenu gs 
            _ -> drawGameState gs

input :: Event -> GameState -> GameState
-- Mouse Move
input (EventMotion (mx, my)) gs = gs {mousePos = (mx + fromIntegral windowWidth / 2, my + fromIntegral windowHeight / 2)}

-- Main Menu
input e gs@GameState {currentState = MainMenu _} = updateMenuInput e gs

-- Mouse Click Down
input (EventKey (MouseButton LeftButton) Down _ _) gs | isNothing . selectedLocation $ gs = case currentState gs of
  ChessMove -> gs {selectedLocation = Just location}
  FourMove -> attemptMove gs location
  FallingDown _ _ -> gs
  GameOver _ -> gs
  MainMenu _ -> gs
  where
    location = getTilePosFromScreenPos (mousePos gs)

-- Mouse Click Up
input (EventKey (MouseButton LeftButton) Up _ _) gs | isJust . selectedLocation $ gs = case currentState gs of
  ChessMove -> attemptMove gs location
  _ -> gs {selectedLocation = Nothing}
  where
    location = getTilePosFromScreenPos (mousePos gs)
input (EventKey (MouseButton LeftButton) Up _ _) gs = case currentState gs of 
  GameOver _ -> gs {currentState = MainMenu False}
  _ -> gs

-- Other
input e gs = trace (show e) gs