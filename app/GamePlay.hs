module GamePlay where

import Data.Maybe (isNothing)
import GameState (Board (..), BoardPiece (ChessPiece, FourPiece), GameState (..), PieceType (..), State (..), getPieceAt, movePiece, setPieceAt, withinBoard, Row, EndState (ChessWon, FourWon), PlayerType (..))

updateFrequency :: Int
updateFrequency = 60

updateInterval :: Float
updateInterval = 1 / fromIntegral updateFrequency

fallInterval :: Float
fallInterval = 0.5

forDirection :: ((Int, Int) -> a) -> Board -> (Int, Int) -> (Int, Int) -> [a]
forDirection f b pos@(x, y) (dx, dy) 
  | withinBoard b pos = f pos : forDirection f b newPos (dx, dy)
  | otherwise = []
  where
    newPos = (x + dx, y + dy)

foldDirection :: (a -> Maybe BoardPiece -> a) -> a -> Board -> (Int, Int) -> (Int, Int) -> a
foldDirection f start b pos@(x, y) (dx, dy)
  | withinBoard b pos = let p = getPieceAt b pos in foldDirection f (f start p) b newPos (dx, dy)
  | otherwise = start
  where
    newPos = (x + dx, y + dy) 

walkDirection :: Board -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
walkDirection b (x, y) (dx, dy)
  | withinBoard b newPos = case getPieceAt b newPos of
      Just _ -> []
      Nothing -> newPos : walkDirection b newPos (dx, dy)
  | otherwise = []
  where
    newPos = (x + dx, y + dy)

walkHorizontal :: Board -> (Int, Int) -> [(Int, Int)]
walkHorizontal b p = walkDirection b p (-1, 0) ++ walkDirection b p (1, 0)

walkVertical :: Board -> (Int, Int) -> [(Int, Int)]
walkVertical b p = walkDirection b p (0, -1) ++ walkDirection b p (0, 1)

walkStraight :: Board -> (Int, Int) -> [(Int, Int)]
walkStraight b p = walkHorizontal b p ++ walkVertical b p

walkDiagonal :: Board -> (Int, Int) -> [(Int, Int)]
walkDiagonal b p = walkDirection b p (-1, -1) ++ walkDirection b p (1, -1) ++ walkDirection b p (-1, 1) ++ walkDirection b p (1, 1)

getValidChessMovesForPosition :: Board -> (Int, Int) -> [(Int, Int)]
getValidChessMovesForPosition b (x, y)
  | Just (ChessPiece Pawn) <- maybePiece = generatePawnMovement
  | Just (ChessPiece Knight) <- maybePiece =
      filter
        isFree
        [ (x + 1, y + 2),
          (x - 1, y + 2),
          (x + 2, y + 1),
          (x - 2, y + 1),
          (x + 1, y - 2),
          (x - 1, y - 2),
          (x + 2, y - 1),
          (x - 2, y - 1)
        ]
  | Just (ChessPiece Bishop) <- maybePiece = walkDiagonal b (x, y)
  | Just (ChessPiece Rook) <- maybePiece = walkStraight b (x, y)
  | Just (ChessPiece Queen) <- maybePiece = walkStraight b (x, y) ++ walkDiagonal b (x, y) -- filterReachable $ [(x + i, y + i) | i <- [-7 .. 7], i /= 0] ++ [(x + i, y - i) | i <- [-7 .. 7], i /= 0] ++ [(x + i, y) | i <- [-7 .. 7], i /= 0] ++ [(x, y + i) | i <- [-7 .. 7], i /= 0]
  | Just (ChessPiece King) <- maybePiece = filter isFree [(x, y + 1), (x + 1, y + 1), (x + 1, y), (x + 1, y - 1), (x, y - 1), (x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]
  | otherwise = []
  where
    maybePiece = getPieceAt b (x, y)

    isFree :: (Int, Int) -> Bool
    isFree = isNothing . getPieceAt b

    generatePawnMovement :: [(Int, Int)]
    generatePawnMovement
      | not $ isFree (x, y + 1) = []
      | y == 1 && isFree (x, y + 2) = [(x, y + 1), (x, y + 2)]
      | otherwise = [(x, y + 1)]

isMoveValid :: GameState -> (Int, Int) -> Bool
isMoveValid (GameState b _ _ (Just sp) ChessMove _ _ _) (x, y) = (x, y) `elem` validMoves
  where
    validMoves = getValidChessMovesForPosition b sp
isMoveValid (GameState b _ _ _ FourMove _ _ _) (x, 7) = isNothing . getPieceAt b $ (x, 7)
isMoveValid (GameState _ _ _ _ FourMove _ _ _) (_, _) = False
isMoveValid _ _ = False

attemptMove :: GameState -> (Int, Int) -> GameState
attemptMove gs@(GameState b _ _ (Just sp) ChessMove _ Player _) dest
  | isMoveValid gs dest = gs {selectedLocation = Nothing, board = movePiece b sp dest, currentState = FallingDown FourMove 0}
  | otherwise = gs {selectedLocation = Nothing}
attemptMove gs@(GameState b _ _ _ FourMove piecesLeft _ Player) dest
  | isMoveValid gs dest = gs {selectedLocation = Nothing, board = setPieceAt b dest $ Just FourPiece, currentState = FallingDown ChessMove 0, piecesLeft = (piecesLeft - 1)}
  | otherwise = gs {selectedLocation = Nothing}
attemptMove gs _ = gs {selectedLocation = Nothing}


didFourWin :: Board -> Bool
didFourWin b = maximum (map snd countHorizontalLines ++ map snd countVerticalLines ++ map snd countDiagonalLines) >= 4
  where    
    countHorizontalLines :: [(Int, Int)]
    countHorizontalLines = forDirection (\p -> countLine p (1, 0)) b (0,0) (0,1)
    countVerticalLines :: [(Int, Int)]
    countVerticalLines = forDirection (\p -> countLine p (0, 1)) b (0,0) (1,0)
    countDiagonalLines :: [(Int, Int)]
    countDiagonalLines = forDirection (\p -> countLine p (1, 1)) b (0,0) (0,1) ++ forDirection (\p -> countLine p (1, 1)) b (0,0) (1,0)
                        ++ forDirection (\p -> countLine p (1, 1)) b (0,boardHeight) (0,-1) ++ forDirection (\p -> countLine p (1, 1)) b (0,boardHeight) (1,0)
        where 
          boardHeight = length . head $ rows b
    countLine :: (Int, Int) -> (Int, Int) -> (Int, Int)
    countLine p d = foldDirection countConsecutivePieces (0, 0) b p d

    countConsecutivePieces :: (Int, Int) -> Maybe BoardPiece -> (Int, Int)
    countConsecutivePieces (i, j) (Just FourPiece) = (i + 1, max j (i + 1))
    countConsecutivePieces (_, j) _ = (0, j)

updateFallingPiecesInRow :: Row -> Row -> (Row, Row, Bool)
updateFallingPiecesInRow [] [] = ([], [], False)
updateFallingPiecesInRow [] _ = error "Rows are not the same length"
updateFallingPiecesInRow _ [] = error "Rows are not the same length"
updateFallingPiecesInRow (Nothing:belowr) ((Just FourPiece):abover) = (Just FourPiece:updatedBelow, Nothing:updateAbove, True)
  where
    (updatedBelow, updateAbove, _) = updateFallingPiecesInRow belowr abover
updateFallingPiecesInRow (b:belowr) (p:abover) = (b:updatedBelow, p:updateAbove, changed)
  where
    (updatedBelow, updateAbove, changed) = updateFallingPiecesInRow belowr abover

updateFallingPieces :: Row -> [Row] -> ([Row], Bool)
updateFallingPieces r [] = ([r], False)
updateFallingPieces curr (next:rows) = (updatedCurr : updatedNextRows, changed' || changed)
  where
    (updatedCurr, updateNext, changed) = updateFallingPiecesInRow curr next
    (updatedNextRows, changed') = updateFallingPieces updateNext rows

updateWorld :: Float -> GameState -> GameState

updateWorld _ gs@(GameState b _ _ _ ChessMove _ AI _) = undefined
updateWorld _ gs@(GameState b _ _ _ FourMove _ _ AI) = undefined

updateWorld _ gs@(GameState b@(Board (r:rows)) _ _ _ (FallingDown nextState' fallTimer) piecesLeft _ _) | newFallTimer <= 0 = gs{board=updatedBoard, currentState=nextState}
                                                                                                        | otherwise = gs{currentState=FallingDown nextState' newFallTimer}
  where
    updatedBoard = Board $ updatedRows
    (updatedRows, changed) = updateFallingPieces r rows
    newFallTimer = fallTimer - updateInterval
    nextState | changed = FallingDown nextState' fallInterval
              | didFourWin b = GameOver FourWon
              | piecesLeft <= 0 = GameOver ChessWon
              | otherwise = nextState'
updateWorld _ gs = gs