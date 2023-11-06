module Graphics.Gtk.Piece (PieceHandle
                          , pieceHandles) where

import qualified Data.Text as T
import qualified GI.Rsvg as Rsvg
import qualified Data.Map as Map

import Data.Maybe
import System.FilePath

import Model.Board.SquareList
import Paths_chess

type PieceHandle = Rsvg.Handle

pieceHandle :: String -> IO PieceHandle
pieceHandle name = do
  dataDir <- getDataDir
  fromMaybe (error (name ++ " has gone missing.")) <$>
    Rsvg.handleNewFromFile (T.pack $ dataDir </> ("assets/" ++ name))

pieceHandles :: IO (Map.Map Piece PieceHandle)
pieceHandles = do
  blackKing <- pieceHandle "kdt.svg"
  blackQueen <- pieceHandle "qdt.svg"
  blackRook <- pieceHandle "rdt.svg"
  blackBishop <- pieceHandle "bdt.svg"
  blackKnight <- pieceHandle "ndt.svg"
  blackPawn <- pieceHandle "pdt.svg"
  whiteKing <- pieceHandle "klt.svg"
  whiteQueen <- pieceHandle "qlt.svg"
  whiteRook <- pieceHandle "rlt.svg"
  whiteBishop <- pieceHandle "blt.svg"
  whiteKnight <- pieceHandle "nlt.svg"
  whitePawn <- pieceHandle "plt.svg"
  return $ Map.fromList [ (Piece King Dark, blackKing)
                      , (Piece Queen Dark, blackQueen)
                      , (Piece Rook Dark, blackRook)
                      , (Piece Bishop Dark, blackBishop)
                      , (Piece Knight Dark, blackKnight)
                      , (Piece Pawn Dark, blackPawn)
                      , (Piece King Light, whiteKing)
                      , (Piece Queen Light, whiteQueen)
                      , (Piece Rook Light, whiteRook)
                      , (Piece Bishop Light, whiteBishop)
                      , (Piece Knight Light, whiteKnight)
                      , (Piece Pawn Light, whitePawn)
                      ]
