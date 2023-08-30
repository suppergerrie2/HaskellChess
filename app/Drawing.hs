module Drawing where

import Data.Foldable (foldlM)
import Data.Map.Strict qualified as Map
import GamePlay (getValidChessMovesForPosition)
import GameState (Board (rows), BoardPiece (ChessPiece, FourPiece), GameState (GameState), ImageData(..), PieceType (..), Row, allBoardPieceTypes, getPieceAt, State (..), ButtonImageData (ButtonImageData))
import Graphics.Gloss (Picture, blank, circle, color, green, greyN, loadBMP, makeColorI, pictures, rectangleSolid, scale, text, translate, withAlpha)
import PictureUtils (drawPictureWithMaxSize, translateCentered)
import Data.Maybe (isNothing)

windowSize :: (Int, Int)
windowSize = (640, 640)

windowWidth :: Int
windowHeight :: Int
(windowWidth, windowHeight) = windowSize

tileSize :: Float
tileSize = fromIntegral windowWidth / 8

rowHeight :: Float
rowHeight = tileSize

loadBoardTypesImages :: IO (Map.Map BoardPiece Picture)
loadBoardTypesImages =
  foldlM loadImageIntoMap Map.empty allBoardPieceTypes
  where
    getPiecePath :: BoardPiece -> FilePath
    getPiecePath (ChessPiece pt) = "assets\\pieces\\128h\\w_" ++ pieceName pt ++ "_png_128px.bmp"
    getPiecePath FourPiece = "assets\\pieces\\four.bmp"
    
    pieceName :: PieceType -> String
    pieceName Pawn = "pawn"
    pieceName Knight = "knight"
    pieceName Bishop = "bishop"
    pieceName Rook = "rook"
    pieceName Queen = "queen"
    pieceName King = "king"

    loadImageIntoMap :: (Map.Map BoardPiece Picture) -> BoardPiece -> IO (Map.Map BoardPiece Picture)
    loadImageIntoMap imageData boardPiece = do
      let path = getPiecePath boardPiece
      putStrLn $ "Loading image: " ++ path
      image <- loadBMP path
      return $ Map.insert boardPiece image imageData

loadButtonImages :: IO (Map.Map String ButtonImageData)
loadButtonImages = foldlM loadButton Map.empty ["play_chess", "play_four", "play_both"] 
  where
    getPaths :: String -> (FilePath, FilePath, FilePath)
    getPaths name = ("assets\\" ++ name ++ "_button.bmp", "assets\\" ++ name ++ "_button_hover.bmp", "assets\\" ++ name ++ "_button_pressed.bmp")

    loadButton :: Map.Map String ButtonImageData -> String -> IO (Map.Map String ButtonImageData)
    loadButton imageData path = do
      let (defaultPath, hoverPath, pressedPath) = getPaths path

      putStrLn $ "Loading button: " ++ path
      putStrLn $ "Default: " ++ defaultPath
      putStrLn $ "Hover: " ++ hoverPath
      putStrLn $ "Pressed: " ++ pressedPath

      defaultImage <- loadBMP defaultPath
      hoverImage <- loadBMP hoverPath
      pressedImage <- loadBMP pressedPath

      return $ Map.insert path (ButtonImageData defaultImage hoverImage pressedImage) imageData


loadImages :: IO ImageData
loadImages = do
  putStrLn "Loading images..."
  boardPieces <- loadBoardTypesImages
  buttonData <- loadButtonImages
  putStrLn "Images loaded!"
  return $ ImageData boardPieces buttonData

rectangleSolidOffset :: Float -> Float -> Picture
rectangleSolidOffset w h = translate (w / 2) (h / 2) $ rectangleSolid w h

circleOffset :: Float -> Picture
circleOffset r = translate r r $ circle r

getTilePosFromScreenPos :: (Float, Float) -> (Int, Int)
getTilePosFromScreenPos (mx, my) = (floor $ mx / tileSize, floor $ my / tileSize)

findPieceAtPos :: (Float, Float) -> GameState -> Maybe BoardPiece
findPieceAtPos (mx, my) (GameState b _ _ _ _ _ _ _) = getPieceAt b tilePos
  where
    tilePos = getTilePosFromScreenPos (mx, my)

drawTile :: Int -> Int -> Picture
drawTile x y = color tileColour $ rectangleSolidOffset tileSize tileSize
  where
    tileColour = if odd (x + y) then greyN 0.9 else greyN 0.1

drawBoardPiece :: ImageData -> BoardPiece -> Picture
drawBoardPiece imageData piece = translate offset offset $ drawPictureWithMaxSize size $ (boardPieces imageData) Map.! piece -- Errors when piece type does not exist in the map, but this is not a problem as without the image data the game won't look as intended anyways.
  where
    size = tileSize * 0.8
    offset = (tileSize - size) / 2

drawRow :: ImageData -> Maybe (Int, Int) -> Row -> Int -> Picture
drawRow imageData selectedLocation r y = pictures $ map (uncurry draw) (zip r [0 ..])
  where
    draw :: Maybe BoardPiece -> Int -> Picture
    draw Nothing x = translate (fromIntegral x * tileSize) 0 $ drawTile x y
    draw (Just p) x
      | maybe False ((==) (x, y)) selectedLocation = translate (fromIntegral x * tileSize) 0 $ drawTile x y
      | otherwise = translate (fromIntegral x * tileSize) 0 $ pictures [drawTile x y, drawBoardPiece imageData p]

drawHoveredTile :: (Float, Float) -> Picture
drawHoveredTile (mx, my) = translate (tileSize / 2 + x) (tileSize / 2 + y) $ color (withAlpha 0.4 green) $ rectangleSolid tileSize tileSize
  where
    (tileX, tileY) = getTilePosFromScreenPos (mx, my)
    x = fromIntegral tileX * tileSize
    y = fromIntegral tileY * tileSize

drawSelectedTile :: Board -> ImageData -> Maybe (Int, Int) -> (Float, Float) -> Picture
drawSelectedTile _ _ Nothing mp = drawHoveredTile mp
drawSelectedTile b imageData (Just (x, y)) (mx, my)
  | Just p <- piece = translateCentered mx my $ drawBoardPiece imageData p
  | otherwise = translate x' y' $ color (withAlpha 0.4 green) $ rectangleSolidOffset tileSize tileSize
  where
    x' = fromIntegral x * tileSize
    y' = fromIntegral y * tileSize
    piece = getPieceAt b (x, y)

drawValidMoves :: State -> Board -> ImageData -> Maybe (Int, Int) -> Picture
drawValidMoves FourMove b _ _ = pictures $ map drawValidPos $ filter isFree $ [(x, 7) | x <- [0 .. 7]]
  where
    isFree :: (Int, Int) -> Bool
    isFree = isNothing . getPieceAt b

    drawValidPos :: (Int, Int) -> Picture
    drawValidPos (x, y) = translate (fromIntegral x * tileSize) (fromIntegral y * tileSize) $ color (withAlpha 0.4 green) $ rectangleSolidOffset tileSize tileSize

drawValidMoves ChessMove _ _ Nothing = blank
drawValidMoves ChessMove b _ (Just (tx, ty)) = pictures $ map drawValidPos (getValidChessMovesForPosition b (tx, ty))
  where
    drawValidPos :: (Int, Int) -> Picture
    drawValidPos (x, y) = translate (fromIntegral x * tileSize) (fromIntegral y * tileSize) $ color (withAlpha 0.4 green) $ rectangleSolidOffset tileSize tileSize

drawValidMoves _ _ _ _ = blank

drawBoard :: ImageData -> Maybe (Int, Int) -> Board -> Picture
drawBoard imageData sl b = pictures $ map (uncurry drawRowWithIndex) (zip (rows b) [0 ..])
  where
    drawRowWithIndex :: Row -> Int -> Picture
    drawRowWithIndex r y = translate 0 (fromIntegral y * rowHeight) (drawRow imageData sl r y)

drawGameState :: GameState -> Picture
drawGameState (GameState b imageData mp sl cp _ _ _) =
  pictures [ 
    drawBoard imageData sl b,
    drawValidMoves cp b imageData sl,
    drawSelectedTile b imageData sl mp,
    translate 0 (fromIntegral windowHeight - 32) $ scale 0.2 0.2 $ color (makeColorI 255 0 0 255) $ text (show cp)
  ]
