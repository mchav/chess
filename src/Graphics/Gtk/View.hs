{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Graphics.Gtk.View (runBoard) where

import qualified GI.Gtk as Gtk
import qualified GI.Rsvg as Rsvg

import Data.GI.Base

import qualified Data.Map as Map

import Data.IORef
import Data.Maybe
import GI.Cairo.Render
import GI.Cairo.Render.Connector
import GI.Gdk

import Graphics.Gtk.Piece
import Model.Board.Representation
import Model.Board.SquareList
import Model.GameState

data Colour = Black | White | Green | Blue

data ViewProperties = ViewProperties {
  viewWidth         :: Double,
  viewHeight        :: Double,
  boardOffsetCoords :: (Double, Double),
  boardSideLength   :: Double
}

runBoard :: IORef (GameState Int) -> IO ()
runBoard gamestate = do
  _ <- Gtk.init Nothing
  handles <- pieceHandles

  drawingArea <- new Gtk.DrawingArea []
  eventBox <- new Gtk.EventBox [ #child := drawingArea ]
  win <- new Gtk.Window [ #type := Gtk.WindowTypeToplevel
                        , #iconName := "applications-haskell"
                        , #defaultWidth := 1024
                        , #defaultHeight := 768
                        , #child := eventBox ]
  _ <- on win #destroy Gtk.mainQuit

  header <- new Gtk.HeaderBar [#showCloseButton := True]
  #setTitlebar win (Just header)

  _ <- Gtk.onWidgetDraw drawingArea $ renderWithContext $ drawBoard handles gamestate drawingArea

  _ <- Gtk.onWidgetButtonPressEvent eventBox (onButtonPressHandler drawingArea gamestate)

  #showAll win

  Gtk.main


onButtonPressHandler :: Gtk.DrawingArea
                     -> IORef (GameState Int)
                     -> EventButton
                     -> IO Bool
onButtonPressHandler drawingArea gameState e = do
  viewProperties <- liftIO $ computeViewProperties drawingArea
  mouseX <- getEventButtonX e
  mouseY <- getEventButtonY e
  let coords = (getMouseClickCoordinates 
                        (boardSideLength viewProperties)
                        (boardOffsetCoords viewProperties)
                        (mouseX, mouseY))
  _ <- liftIO (print coords)
  state <- readIORef gameState
  let prevSquare = selectedSquare state
  let updatedState = state { selectedSquare = (Just coords),
                             previouslySelectedSquare = prevSquare }
  writeIORef gameState updatedState
  Gtk.widgetQueueDraw drawingArea
  return True

drawBoard :: Map.Map Piece PieceHandle -> IORef (GameState Int) -> Gtk.DrawingArea -> Render Bool
drawBoard handles gameState drawingArea = do
  viewProperties <- liftIO (computeViewProperties drawingArea)
  let (boardX, boardY) = boardOffsetCoords viewProperties
  let gridSide = boardSideLength viewProperties
  drawGrid handles gameState boardX boardY gridSide
  return True

drawGrid :: Map.Map Piece PieceHandle -> IORef (GameState Int) -> Double -> Double -> Double -> Render ()
drawGrid handles gameState boardX boardY side = do
  state <- liftIO $ readIORef gameState
  let previousBoard = toSquareList $ board state
  let sc = selectedSquare state
  let previous = previouslySelectedSquare state
  let newBoardState = movePiece previous sc previousBoard
  let wasUpdated = newBoardState /= previousBoard
  let updatedState = state { board = (fromSquareList newBoardState), previouslySelectedSquare = if wasUpdated then Nothing else previous, selectedSquare = if wasUpdated then Nothing else sc }
  _ <- liftIO $ writeIORef gameState updatedState
  let squareSize = side / 8
  let gridWithColours = map (\(coords, colour) -> if (fromMaybe False (fmap ((==) coords) sc)) then (coords, Blue) else (coords, colour)) colouredGrid
  let gridWithSquares = gridPoints `zip` (concat newBoardState)
  mapM_ (\((offsetX, offsetY), colour) ->
    let xCoord = boardX + (fromIntegral offsetX) * squareSize
        yCoord = boardY + (fromIntegral offsetY) * squareSize
    in drawSquare colour xCoord yCoord squareSize) gridWithColours
  mapM_ (\((offsetX, offsetY), square) ->
    let xCoord = boardX + (fromIntegral offsetX) * squareSize
        yCoord = boardY + (fromIntegral offsetY) * squareSize
    in drawPiece handles square xCoord yCoord squareSize) gridWithSquares

drawSquare :: Colour -> Double -> Double -> Double -> Render ()
drawSquare colour startX startY side = do
  case colour of
    Black -> setSourceRGB 0.4 0.4 0.4
    White -> setSourceRGB 1.0 1.0 1.0
    Green -> setSourceRGB 0.0 1.0 0.0
    Blue  -> setSourceRGB 0.0 0.0 1.0
  rectangle startX startY side side
  fill

drawPiece :: Map.Map Piece PieceHandle -> Square -> Double -> Double -> Double -> Render ()
drawPiece handles square squareX squareY side = do
  case square of
    Empty        -> return ()
    Square piece -> do
        rect <- Gtk.new Rsvg.Rectangle
               [#height := side, #width := side, #x := squareX, #y := squareY]
        case Map.lookup piece handles of
          Nothing -> return ()
          Just p  -> do
              ctx <- getContext
              Rsvg.handleRenderDocument p ctx rect


computeViewProperties :: Gtk.DrawingArea -> IO ViewProperties
computeViewProperties drawingArea = do
  w <- fromIntegral <$> Gtk.widgetGetAllocatedWidth drawingArea
  h <- fromIntegral <$> Gtk.widgetGetAllocatedHeight drawingArea
  let minWindowDimension = min w h
  let gridSide = 0.8 * minWindowDimension
  return ViewProperties { viewWidth = w
                        , viewHeight = h
                        , boardOffsetCoords = ( (w - gridSide) / 2.0
                                              , (h - gridSide) / 2.0)
                        , boardSideLength = 0.8 * minWindowDimension
                      }

getMouseClickCoordinates :: Double
                         -> (Double, Double)
                         -> (Double, Double)
                         -> (Int, Int)
getMouseClickCoordinates gridSide (bX, bY) (mX, mY) = (
    floor $ (mX - bX) / (gridSide / 8.0),
    floor $ (mY - bY) / (gridSide / 8.0)
  )

gridPoints :: [(Int, Int)]
gridPoints = [(i, j) | i <- [0..7], j <- [0..7]]

colouredGrid :: [((Int, Int), Colour)]
colouredGrid = [(i, j) | i <- [0..7], j <- [0..7]] `zip` colours
  where colours = cycle $ (take 8 (cycle [White, Black])) ++ (take 8 (cycle [Black, White]))
