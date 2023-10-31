module Graphics.Sdl.Piece (PieceHandle
                          , pieceHandles) where

import qualified Data.Map as Map
import qualified SDL.Image as Image

import System.FilePath
import SDL

import Model.Board.SquareList

type PieceHandle = Texture

loadTexture :: Renderer -> String -> IO Texture
loadTexture r s = do
  svg <- Image.load $ "/usr/local/google/home/mchavinda/code/haskell/chess" </> ("./assets/" ++ s)
  SDL.createTextureFromSurface r svg <* SDL.freeSurface svg

pieceHandles :: Renderer -> IO (Map.Map Piece PieceHandle)
pieceHandles r = do
  blackKing <- loadTexture r "kdt.svg"
  blackQueen <- loadTexture r "qdt.svg"
  blackRook <- loadTexture r "rdt.svg"
  blackBishop <- loadTexture r "bdt.svg"
  blackKnight <- loadTexture r "ndt.svg"
  blackPawn <- loadTexture r "pdt.svg"
  whiteKing <- loadTexture r "klt.svg"
  whiteQueen <- loadTexture r "qlt.svg"
  whiteRook <- loadTexture r "rlt.svg"
  whiteBishop <- loadTexture r "blt.svg"
  whiteKnight <- loadTexture r "nlt.svg"
  whitePawn <- loadTexture r "plt.svg"
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
