module GameState where
import Graphics.Gloss (Picture)
import qualified Data.Map.Strict as Map

type ImageData = Map.Map BoardPiece Picture

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Show)

allPieceTypes :: [PieceType]
allPieceTypes = [Pawn, Knight, Bishop, Rook, Queen, King]

data Player = White | Black
  deriving (Eq, Ord, Show)

data BoardPiece = PlayerPiece {player :: Player, piece :: PieceType}
  deriving (Eq, Ord, Show)

allBoardPieceTypes :: [BoardPiece]
allBoardPieceTypes = (map (PlayerPiece White) allPieceTypes ++ map (PlayerPiece Black) allPieceTypes)

type Row = [Maybe BoardPiece]

data Board = Board {rows :: [Row]}

data GameState = GameState {board :: Board, images :: ImageData, mousePos :: (Float, Float), selectedLocation :: Maybe (Int, Int)}



initPawnRow :: Player -> Row
initPawnRow = replicate 8 . Just . flip PlayerPiece Pawn

initPieceRow :: Player -> Row
initPieceRow p = [
  Just $ PlayerPiece p Rook,
  Just $ PlayerPiece p Knight,
  Just $ PlayerPiece p Bishop,
  Just $ PlayerPiece p Queen,
  Just $ PlayerPiece p King,
  Just $ PlayerPiece p Bishop,
  Just $ PlayerPiece p Knight,
  Just $ PlayerPiece p Rook
  ]

initBoard :: Board
initBoard =
  Board
    [ initPieceRow White,
      initPawnRow White,
      replicate 8 Nothing,
      replicate 8 Nothing,
      replicate 8 Nothing,
      replicate 8 Nothing,
      initPawnRow Black,
      initPieceRow Black
    ]
  
initGameState :: ImageData -> GameState
initGameState imageData = GameState initBoard imageData (0, 0) Nothing

getPieceAt :: Board -> (Int, Int) -> Maybe BoardPiece
getPieceAt (Board rows) (_, y) | y < 0 || y >= length rows = Nothing
getPieceAt (Board rows) (x, y) | x < 0 || x >= length row = Nothing
  where
    row = rows !! y
getPieceAt (Board rows) (x, y) = (rows !! y) !! x