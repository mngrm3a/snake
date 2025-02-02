module Snake.Event (handleEvent) where

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid (First (First, getFirst))
import Data.Sequence ((|>))
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Lens.Micro.Platform ((&), (.~), (^.))
import Snake.World (Status (..), World, keys, state, status)

handleEvent :: Gloss.Event -> World -> World
handleEvent (Gloss.EventKey key Gloss.Down _ _) w =
  let newKey = case w ^. state . status of
        GetReady -> filterKeys key $ const True
        Playing -> filterKeys key isArrowKey
        Collision -> filterKeys key $ const True
      curKeys = Just $ w ^. state . keys
      newKeys = getFirst $ First ((|>) <$> curKeys <*> newKey) <> First curKeys
   in w & state . keys .~ fromMaybe mempty newKeys
handleEvent _ w = w

filterKeys :: Gloss.Key -> (Gloss.Key -> Bool) -> Maybe Gloss.Key
filterKeys key p = bool Nothing (Just key) $ p key

isArrowKey :: Gloss.Key -> Bool
isArrowKey key = case key of
  (Gloss.SpecialKey specialKey) -> specialKey `elem` arrowKeys
  _anyOtherKey -> False
  where
    arrowKeys = [Gloss.KeyUp, Gloss.KeyDown, Gloss.KeyRight, Gloss.KeyLeft]