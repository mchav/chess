module Model.GameState ( GameState(..)
                       , initialGameState) where

import Model.Board.Representation (Board(..))
import Model.Board.SquareList (startingBoardPosition)

data GameState = GameState {
    previouslySelectedSquare :: Maybe (Int, Int),
    selectedSquare :: Maybe (Int, Int),
    board :: Board
  }

initialGameState :: GameState
initialGameState = GameState {
    previouslySelectedSquare = Nothing,
    selectedSquare = Nothing,
    board = SquareList startingBoardPosition
  }