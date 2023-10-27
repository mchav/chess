module Model.Board.SquareList
  ( PieceType (..)
  , PieceColour (..)
  , Piece (..)
  , Square (..)
  , startingBoardPosition
  , movePiece) where

import qualified Data.List as List
import qualified Data.List.Split as Split

data PieceType = King | Queen | Bishop | Rook | Knight | Pawn
  deriving (Eq, Ord, Show)

data PieceColour = Dark | Light
  deriving (Eq, Ord, Show)

data Piece = Piece PieceType PieceColour
  deriving (Eq, Ord, Show)

data Square = Square Piece | Empty
  deriving (Eq, Show)

startingBoardPosition :: [[Square]]
startingBoardPosition = List.transpose [ [Square (Piece Rook Dark), Square (Piece Knight Dark), Square (Piece Bishop Dark), Square (Piece Queen Dark), Square (Piece King Dark), Square (Piece Bishop Dark), Square (Piece Knight Dark), Square (Piece Rook Dark)]
                        , [Square (Piece Pawn Dark), Square (Piece Pawn Dark), Square (Piece Pawn Dark), Square (Piece Pawn Dark), Square (Piece Pawn Dark), Square (Piece Pawn Dark), Square (Piece Pawn Dark), Square (Piece Pawn Dark)]
                        , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                        , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                        , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                        , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                        , [Square (Piece Pawn Light), Square (Piece Pawn Light), Square (Piece Pawn Light), Square (Piece Pawn Light), Square (Piece Pawn Light), Square (Piece Pawn Light), Square (Piece Pawn Light), Square (Piece Pawn Light)]
                        , [Square (Piece Rook Light), Square (Piece Knight Light), Square (Piece Bishop Light), Square (Piece Queen Light), Square (Piece King Light), Square (Piece Bishop Light), Square (Piece Knight Light), Square (Piece Rook Light)]
                        ]

movePiece :: Maybe (Int, Int) -> Maybe (Int, Int) -> [[Square]] -> [[Square]]
movePiece Nothing _ board             = board
movePiece _ Nothing board             = board
movePiece (Just from) (Just to) board = Split.chunksOf 8 $ map (\(point, piece) -> if point == to then pieceAtSquare else (if point == from then Empty else piece)) boardWithPoints
  where gridPoints = [(x, y) | x <- [0..7], y <- [0..7]]
        boardWithPoints = gridPoints `zip` (concat board)
        pieceAtSquare = headOr Empty $ map snd $ filter (((==) from) . fst) boardWithPoints


headOr :: a -> [a] -> a
headOr _ (x:xs)       = x
headOr defaultValue _ = defaultValue