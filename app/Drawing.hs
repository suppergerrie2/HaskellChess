module Drawing where

import GameState(GameState (GameState), BoardPiece (PlayerPiece), ImageData, Row, Board (rows), getPieceAt, PieceType (..), Player (..), allBoardPieceTypes)
import Graphics.Gloss (Picture, translate, rectangleSolid, circle, color, greyN, pictures, green, withAlpha, loadBMP, blank)
import PictureUtils (drawPictureWithMaxSize, translateCentered)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Data.Foldable (foldlM)
import Data.Maybe (isJust)

windowSize :: (Int, Int)
windowSize = (640, 640)

windowWidth :: Int
windowHeight :: Int
(windowWidth, windowHeight) = windowSize

tileSize :: Float
tileSize = fromIntegral windowWidth / 8

rowHeight :: Float
rowHeight = tileSize

loadImages :: IO ImageData
loadImages =
  foldlM loadImageIntoImageData Map.empty allBoardPieceTypes
  where
    getPiecePath :: BoardPiece -> FilePath
    getPiecePath (PlayerPiece p pt) = "assets\\pieces\\128h\\" ++ prefix p ++ "_" ++ pieceName pt ++ "_png_128px.bmp"

    pieceName :: PieceType -> String
    pieceName Pawn = "pawn"
    pieceName Knight = "knight"
    pieceName Bishop = "bishop"
    pieceName Rook = "rook"
    pieceName Queen = "queen"
    pieceName King = "king"

    prefix :: Player -> String
    prefix White = "w"
    prefix Black = "b"

    loadImageIntoImageData :: ImageData -> BoardPiece -> IO ImageData
    loadImageIntoImageData imageData boardPiece = do
      let path = getPiecePath boardPiece
      image <- loadBMP path
      return $ Map.insert boardPiece image imageData

rectangleSolidOffset :: Float -> Float -> Picture
rectangleSolidOffset w h = translate (w / 2) (h / 2) $ rectangleSolid w h

circleOffset :: Float -> Picture
circleOffset r = translate r r $ circle r

getTilePosFromScreenPos :: (Float, Float) -> (Int, Int)
getTilePosFromScreenPos (mx, my) = (floor $ mx / tileSize, floor $ my / tileSize)

findPieceAtPos :: (Float, Float) -> GameState -> Maybe BoardPiece
findPieceAtPos (mx, my) (GameState b _ _ _) = getPieceAt b tilePos
  where
    tilePos = getTilePosFromScreenPos (mx, my)

drawTile :: Int -> Int -> Picture
drawTile x y = color tileColour $ rectangleSolidOffset tileSize tileSize
  where
    tileColour = if even (x + y) then greyN 0.9 else greyN 0.1

drawBoardPiece :: ImageData -> BoardPiece -> Picture
drawBoardPiece imageData piece = translate offset offset $ drawPictureWithMaxSize size $ imageData Map.! piece -- Errors when piece type does not exist in the map, but this is not a problem as without the image data the game won't look as intended anyways.
  where
    size = tileSize * 0.8
    offset = (tileSize - size) / 2

drawRow :: ImageData -> Maybe (Int, Int)  -> Row -> Int -> Picture
drawRow imageData selectedLocation r y = pictures $ map (uncurry draw) (zip r [0 ..])
  where
    draw :: Maybe BoardPiece -> Int -> Picture
    draw Nothing x = translate (fromIntegral x * tileSize) 0 $ drawTile x y
    draw (Just p) x | maybe False ((==) (x, y)) selectedLocation = translate (fromIntegral x * tileSize) 0 $ drawTile x y -- translate (fromIntegral x * tileSize) 0 $ pictures [drawTile x y, color green $ drawBoardPiece imageData p]
                    | otherwise = translate (fromIntegral x * tileSize) 0 $ pictures [drawTile x y, drawBoardPiece imageData p]

drawHoveredTile :: (Float, Float) -> Picture
drawHoveredTile (mx, my) = translate (tileSize/2 + x) (tileSize/2 + y) $ color (withAlpha 0.4 green) $ rectangleSolid tileSize tileSize
  where
    (tileX, tileY) = getTilePosFromScreenPos (mx, my)
    x = fromIntegral tileX * tileSize
    y = fromIntegral tileY * tileSize

drawSelectedTile :: Board -> ImageData -> Maybe (Int, Int) -> (Float, Float) -> Picture
drawSelectedTile _ _ Nothing mp = drawHoveredTile mp
drawSelectedTile b imageData (Just (x, y)) (mx, my) | Just p <- piece = translateCentered mx my $ drawBoardPiece imageData p 
                                   | otherwise = translate x' y' $ color (withAlpha 0.4 green) $ rectangleSolidOffset tileSize tileSize
  where
    x' = fromIntegral x * tileSize
    y' = fromIntegral y * tileSize
    piece = getPieceAt b (x, y)

drawGameState :: GameState -> Picture
drawGameState (GameState b imageData mp sl) = pictures $ 
  map (uncurry drawRowWithIndex) (zip (rows b) [0 ..]) ++
  [
    drawSelectedTile b imageData sl mp
  ]
  where
    drawRowWithIndex :: Row -> Int -> Picture
    drawRowWithIndex r y = translate 0 (fromIntegral y * rowHeight) (drawRow imageData sl r y)