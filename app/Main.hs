module Main (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
  windowPosition <- getCenteredWindowPosition windowSize
  Gloss.play
    (Gloss.InWindow windowName windowSize windowPosition)
    windowColor
    windowFPS
    -- initial world
    ()
    -- render world
    (const Gloss.Blank)
    -- handle events
    (const id)
    -- update world
    (const id)
  where
    windowFPS = 30
    windowName = "Snake"
    windowSize = (800, 600)
    windowColor = Gloss.white

getCenteredWindowPosition :: (Int, Int) -> IO (Int, Int)
getCenteredWindowPosition (winWidth, winHeight) =
  getScreenSize <&> \(scrWidth, scrHeight) ->
    (scrWidth - winWidth & flip div 2, scrHeight - winHeight & flip div 2)
