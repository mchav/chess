import Data.IORef

import qualified Graphics.Gtk.View as Gtk
import qualified Graphics.Sdl.View as Sdl

import Model.GameState (initialGameState)


main :: IO ()
main = do
  gameState <- newIORef initialGameState
  Sdl.runBoard gameState
