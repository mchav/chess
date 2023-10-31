module Model.GameState ( GameState(..)
                       , initialGameState) where

import Model.Board.Representation (Board(..))
import Model.Board.SquareList (startingBoardPosition)

data GameState a = GameState {
    previouslySelectedSquare :: Maybe (a, a),
    selectedSquare :: Maybe (a, a),
    board :: Board
  }

initialGameState :: GameState a
initialGameState = GameState {
    previouslySelectedSquare = Nothing,
    selectedSquare = Nothing,
    board = SquareList startingBoardPosition
  }
