import Data.IORef

import Model.GameState (initialGameState)
import Graphics.Gtk.View (runBoard)

main :: IO ()
main = do
  gameState <- newIORef initialGameState
  runBoard gameState