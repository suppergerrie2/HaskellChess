module Menu where
import Graphics.Gloss.Interface.IO.Game (Event (..), Picture, pictures, translate, Key (..), MouseButton (..), KeyState (..))
import GameState (GameState (..), ImageData (..), ButtonImageData (..), State (MainMenu, ChessMove), PlayerType (AI, Player), initBoard)
import Drawing (drawBoard, tileSize)
import PictureUtils (drawPictureWithSize)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Data.Foldable (find)

updateMenuInput :: Event -> GameState -> GameState
-- Mouse Click Down
updateMenuInput (EventKey (MouseButton LeftButton) Down _ _) gs = gs { currentState = MainMenu True }

-- Mouse Click Up
updateMenuInput (EventKey (MouseButton LeftButton) Up _ _) gs = case buttonOver of 
    Just (_, _, _, callback) -> callback gs 
    Nothing -> gs { currentState = MainMenu False }
    where
        (mx, my) = mousePos gs
        buttonOver = find (\(_, (x, y), (w, h), _) -> mx >= x && mx <= x + w && my >= y && my <= y + h) menu_buttons

updateMenuInput _ gs = gs

drawButton :: ButtonImageData -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool -> Picture
drawButton buttonImageData (x, y) (w, h) (mx, my) pressed | mx >= x && mx <= x + w && my >= y && my <= y + h = translate x y $ drawPictureWithSize (w, h) hoveredImage
                                                          | otherwise = translate x y $ drawPictureWithSize (w, h) $ defaultImage buttonImageData
    where
        hoveredImage | pressed = pressedImage buttonImageData
                     | otherwise = hoverImage buttonImageData

play_chess_pressed :: GameState -> GameState
play_chess_pressed gs = gs { chessPlayer = Player, fourPlayer = AI, currentState = ChessMove, board = initBoard }

play_four_pressed :: GameState -> GameState
play_four_pressed gs = gs { chessPlayer = AI, fourPlayer = Player, currentState = ChessMove, board = initBoard }

play_both_pressed :: GameState -> GameState
play_both_pressed gs = gs { chessPlayer = Player, fourPlayer = Player, currentState = ChessMove, board = initBoard }

buttonHeight :: Float
buttonHeight = tileSize * 1
buttonWidth :: Float
buttonWidth = buttonHeight * 4

menu_buttons :: [(String, (Float, Float), (Float, Float), GameState -> GameState)]
menu_buttons = [
    -- ("play_chess", (tileSize * 2, tileSize * 3), (buttonWidth, buttonHeight), play_chess_pressed),
    -- ("play_four", (tileSize * 2, tileSize * 3 + buttonHeight), (buttonWidth, buttonHeight), play_four_pressed),
    ("play_both", (tileSize * 2, tileSize * 4), (buttonWidth, buttonHeight), play_both_pressed)
  ]

drawMenu :: GameState -> Picture
drawMenu (GameState b imageData mp sl (MainMenu pressed) _ _ _) = pictures $ drawBoard imageData sl b : map drawButtonData menu_buttons
    where
        buttonImageData :: String -> ButtonImageData
        buttonImageData path = (buttons imageData) Map.! path

        drawButtonData :: (String, (Float, Float), (Float, Float), GameState -> GameState) -> Picture
        drawButtonData (path, pos, size, _) = drawButton (buttonImageData path) pos size mp pressed
drawMenu gs = trace ("drawMenu called with invalid gamestate " ++ (show $ currentState gs)) $ pictures []