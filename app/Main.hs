module Main (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Snake.Event (handleEvent)
import Snake.Render (renderWorld)
import Snake.Update (updateWorld)
import Snake.World (mkWorld)

main :: IO ()
main = do
  windowPosition <- getCenteredWindowPosition windowSize
  Gloss.play
    (Gloss.InWindow windowName windowSize windowPosition)
    windowColor
    windowFPS
    (mkWorld windowSize segmentSize)
    renderWorld
    handleEvent
    updateWorld
  where
    windowFPS = 30
    windowName = "Snake"
    windowSize = (800, 600)
    windowColor = Gloss.white
    segmentSize = 20

getCenteredWindowPosition :: (Int, Int) -> IO (Int, Int)
getCenteredWindowPosition (winWidth, winHeight) =
  getScreenSize <&> \(scrWidth, scrHeight) ->
    (scrWidth - winWidth & flip div 2, scrHeight - winHeight & flip div 2)
