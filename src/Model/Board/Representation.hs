module Model.Board.Representation ( Board(..)
                                  , toSquareList
                                  , fromSquareList) where

import Model.Board.SquareList

data Board = SquareList [[Square]]

toSquareList :: Board -> [[Square]]
toSquareList (SquareList squareList) = squareList

fromSquareList :: [[Square]] -> Board
fromSquareList list = SquareList list