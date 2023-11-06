{-# LANGUAGE OverloadedStrings #-}
module Graphics.Sdl.View (runBoard) where

import qualified Data.Map as Map

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.StateVar
import Foreign.C.Types
import SDL.Vect
import SDL

import Graphics.Sdl.Piece
import Model.Board.Representation
import Model.Board.SquareList
import Model.GameState

data Colour = Black | White | Green | Blue

data ViewProperties a = ViewProperties {
  viewWidth         :: a,
  viewHeight        :: a,
  boardOffsetCoords :: (a, a),
  boardSideLength   :: a
}

runBoard :: IORef (GameState CInt) -> IO ()
runBoard gameState = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "Chess"
      SDL.defaultWindow {SDL.windowInitialSize = V2 1024 768}
  SDL.showWindow window
  renderer <- createRenderer window (-1) defaultRenderer
  handles <- pieceHandles renderer
  appLoop gameState handles renderer window
  destroyWindow window

appLoop :: IORef (GameState CInt) -> Map.Map Piece PieceHandle -> Renderer -> SDL.Window -> IO ()
appLoop gameState handles renderer window = do
  viewProperties <- computeViewProperties window
  event <- SDL.pollEvent

  let quit = fromMaybe False $ fmap (((==) SDL.QuitEvent) . SDL.eventPayload) event
  let mouseEvent = event >>= getMouseClickCoordinates 
  let mouseCoords = fmap (coordinatesToGridPosition (boardSideLength viewProperties) (boardOffsetCoords viewProperties)) mouseEvent

  state <- readIORef gameState
  let prevSquare = selectedSquare state
  let updatedState = case mouseCoords of
        Nothing -> state
        _       -> state { selectedSquare = mouseCoords,
                             previouslySelectedSquare = prevSquare }
  writeIORef gameState updatedState

  drawBoard handles gameState viewProperties renderer

  SDL.present renderer

  unless quit (appLoop gameState handles renderer window)

getMouseClickCoordinates :: SDL.Event -> Maybe (CInt, CInt)
getMouseClickCoordinates event = case SDL.eventPayload event of
  MouseButtonEvent e -> Just $ pointToTuple $ mouseButtonEventPos e
  _                  -> Nothing

coordinatesToGridPosition :: CInt
                         -> (CInt, CInt)
                         -> (CInt, CInt)
                         -> (CInt, CInt)
coordinatesToGridPosition gridSide (bX, bY) (mX, mY) = (
    (mX - bX) `div` (gridSide `div` 8),
    (mY - bY) `div` (gridSide `div` 8)
  )

pointToTuple :: (Integral a) => Point V2 a -> (CInt, CInt)
pointToTuple (P (V2 x y)) = (fromIntegral x, fromIntegral y)

toTuple :: V2 a -> (a, a)
toTuple (V2 x y) = (x, y)

computeViewProperties :: SDL.Window -> IO (ViewProperties CInt)
computeViewProperties window = do
  (w, h) <- liftM toTuple (get $ SDL.windowSize window)
  let minWindowDimension = min w h
  let gridSide = floor $ 0.8 * (fromIntegral minWindowDimension)
  return ViewProperties { viewWidth = w
                        , viewHeight = h
                        , boardOffsetCoords = ( (w - gridSide) `div` 2
                                              , (h - gridSide) `div` 2)
                        , boardSideLength = floor $ 0.8 * (fromIntegral minWindowDimension)
                      }

drawBoard :: Map.Map Piece PieceHandle -> IORef (GameState CInt) -> ViewProperties CInt -> Renderer -> IO ()
drawBoard handles gameState viewProperties renderer = do
  let (boardX, boardY) = boardOffsetCoords viewProperties
  let gridSide = boardSideLength viewProperties
  drawGrid handles gameState boardX boardY gridSide renderer

drawGrid :: Map.Map Piece PieceHandle -> IORef (GameState CInt) -> CInt -> CInt -> CInt -> Renderer -> IO ()
drawGrid handles gameState boardX boardY side renderer = do
  SDL.rendererDrawColor renderer $= V4 40 40 40 maxBound
  SDL.clear renderer
  state <- readIORef gameState
  let previousBoard = toSquareList $ board state
  let sc = selectedSquare state
  let previous = previouslySelectedSquare state
  let newBoardState = movePiece previous sc previousBoard
  let wasUpdated = newBoardState /= previousBoard
  let updatedState = state { board = (fromSquareList newBoardState), previouslySelectedSquare = if wasUpdated then Nothing else previous, selectedSquare = if wasUpdated then Nothing else sc }
  writeIORef gameState updatedState
  let squareSize = side `div` 8
  let gridWithColours = map (\(coords, colour) -> if (fromMaybe False (fmap ((==) coords) sc)) then (coords, Blue) else (coords, colour)) colouredGrid
  let gridWithSquares = gridPoints `zip` (concat newBoardState)
  mapM_ (\((offsetX, offsetY), colour) ->
    let xCoord = boardX + (fromIntegral offsetX) * squareSize
        yCoord = boardY + (fromIntegral offsetY) * squareSize
    in drawSquare renderer colour xCoord yCoord squareSize) gridWithColours
  mapM_ (\((offsetX, offsetY), square) ->
    let xCoord = boardX + (fromIntegral offsetX) * squareSize
        yCoord = boardY + (fromIntegral offsetY) * squareSize
    in drawPiece handles renderer square xCoord yCoord squareSize) gridWithSquares

drawSquare :: Renderer -> Colour -> CInt -> CInt -> CInt -> IO ()
drawSquare renderer colour startX startY side = do
  case colour of
    Black -> SDL.rendererDrawColor renderer $= V4  85  85  85 maxBound
    White -> SDL.rendererDrawColor renderer $= V4 170 170 170 maxBound
    Green -> SDL.rendererDrawColor renderer $= V4   0 100   0 maxBound
    Blue  -> SDL.rendererDrawColor renderer $= V4   0   0 100 maxBound
  SDL.fillRect renderer (Just $ SDL.Rectangle (P $ V2 startX startY) (V2 side side))

drawPiece :: Map.Map Piece PieceHandle -> Renderer -> Square -> CInt -> CInt -> CInt -> IO ()
drawPiece handles renderer square squareX squareY side = do
  case square of
    Empty        -> return ()
    Square piece -> do
        case Map.lookup piece handles of
          Nothing -> return ()
          Just p  -> do
              SDL.copy renderer p Nothing (Just $ SDL.Rectangle (P $ V2 (squareX) (squareY)) (V2 side side))


gridPoints :: [(CInt, CInt)]
gridPoints = [(i, j) | i <- [0..7], j <- [0..7]]

colouredGrid :: [((CInt, CInt), Colour)]
colouredGrid = [(i, j) | i <- [0..7], j <- [0..7]] `zip` colours
  where colours = cycle $ (take 8 (cycle [White, Black])) ++ (take 8 (cycle [Black, White]))

