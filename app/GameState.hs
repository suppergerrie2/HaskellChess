module GameState where

import Data.Map.Strict qualified as Map
import Graphics.Gloss (Picture)

-- data Button = Button {buttonPos :: (Float, Float), buttonSize :: (Float, Float), buttonText :: String, onPressed :: GameState -> GameState, defaultImage :: Picture, hoverImage :: Picture, pressedImage :: Picture}

data ButtonImageData = ButtonImageData {defaultImage :: Picture, hoverImage :: Picture, pressedImage :: Picture}
data ImageData = ImageData {boardPieces :: Map.Map BoardPiece Picture, buttons :: Map.Map String ButtonImageData}

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Show)

allPieceTypes :: [PieceType]
allPieceTypes = [Pawn, Knight, Bishop, Rook, Queen, King]

data PlayerType = Player | AI
  deriving (Eq, Ord, Show)

data EndState = ChessWon | FourWon | Draw
  deriving (Eq, Ord, Show)

data State = MainMenu Bool | ChessMove | FourMove | FallingDown State Float | GameOver EndState
  deriving (Eq, Ord, Show)

data BoardPiece = ChessPiece {piece :: PieceType} | FourPiece
  deriving (Eq, Ord, Show)

allBoardPieceTypes :: [BoardPiece]
allBoardPieceTypes = FourPiece : map ChessPiece allPieceTypes

type Row = [Maybe BoardPiece]

data Board = Board {rows :: [Row]}

data GameState = GameState {board :: Board, images :: ImageData, mousePos :: (Float, Float), selectedLocation :: Maybe (Int, Int), currentState :: State, piecesLeft :: Int, chessPlayer :: PlayerType, fourPlayer :: PlayerType}

initPawnRow :: Row
initPawnRow = replicate 8 . Just $ ChessPiece Pawn

initChessRow :: Row
initChessRow =
  [ Just $ ChessPiece Rook,
    Just $ ChessPiece Knight,
    Just $ ChessPiece Bishop,
    Just $ ChessPiece Queen,
    Just $ ChessPiece King,
    Just $ ChessPiece Bishop,
    Just $ ChessPiece Knight,
    Just $ ChessPiece Rook
  ]

initBoard :: Board
initBoard =
  Board
    [ initChessRow,
      initPawnRow,
      replicate 8 Nothing,
      replicate 8 Nothing,
      replicate 8 Nothing,
      replicate 8 Nothing,
      replicate 8 Nothing,
      replicate 8 Nothing
    ]

startPieceCount :: Int
startPieceCount = 15

initGameState :: ImageData -> PlayerType -> PlayerType -> GameState
initGameState imageData chessPlayer fourPlayer = GameState initBoard imageData (0, 0) Nothing (MainMenu False) startPieceCount chessPlayer fourPlayer

withinBoard :: Board -> (Int, Int) -> Bool
withinBoard (Board rows) (x, y) = y >= 0 && y < length rows && x >= 0 && x < length row
  where
    row = rows !! y

getPieceAt :: Board -> (Int, Int) -> Maybe BoardPiece
getPieceAt (Board rows) (_, y) | y < 0 || y >= length rows = Nothing
getPieceAt (Board rows) (x, y) | x < 0 || x >= length row = Nothing
  where
    row = rows !! y
getPieceAt (Board rows) (x, y) = (rows !! y) !! x

setAtIndex :: [a] -> Int -> a -> [a]
setAtIndex list index value = take index list ++ value : drop (index + 1) list

setPieceInRow :: Row -> Int -> Maybe BoardPiece -> Row
setPieceInRow row x p = setAtIndex row x p

setPieceAt :: Board -> (Int, Int) -> Maybe BoardPiece -> Board
setPieceAt b p@(x, y) bp | withinBoard b p = b {rows= setAtIndex (rows b) y (setPieceInRow (rows b !! y) x bp)}
                         | otherwise = b

movePiece :: Board -> (Int, Int) -> (Int, Int) -> Board
movePiece b from to = newBoard
  where
    newBoard = setPieceAt (setPieceAt b from Nothing) to (getPieceAt b from)