{-# LANGUAGE TypeApplications #-}

module Input (handleEvent) where

import Apecs
import Apecs.Gloss
import Apecs.Physics.Gloss
import Collisions (isInsideInteractionBox)
import Components
import Constants (defaultRectSizeV2)
import Control.Monad (when)
import qualified Control.Monad
import DataTypes (DrawLevels (..), EntityState (Idle))
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Interact
import Linear (V2 (V2))
import Particles (spawnParticles)
import Utils (gget)
import Villagers (requestHaulers)

handleEvent :: Event -> System' ()
handleEvent (EventMotion (x, y)) = set global $ MousePosition (V2 x y)

handleEvent (EventKey (SpecialKey KeyF10) Down _ _) = do
  drawLevel <- gget @DrawLevel
  case drawLevel of
    DrawLevel DrawParticles -> set global $ DrawLevel Default
    _ -> set global $ DrawLevel DrawParticles

handleEvent (EventKey (SpecialKey KeyF11) Down _ _) = do
  drawLevel <- gget @DrawLevel
  case drawLevel of
    DrawLevel DrawCollision -> set global $ DrawLevel Default
    _ -> set global $ DrawLevel DrawCollision

handleEvent (EventKey (SpecialKey KeyF9) Down _ _) = do
  drawLevel <- gget @DrawLevel
  case drawLevel of
    DrawLevel DrawAll -> set global $ DrawLevel Default
    _ -> set global $ DrawLevel DrawAll

handleEvent (EventKey (SpecialKey KeyF7) Down _ _) = changeIdlePoint 3 1
handleEvent (EventKey (SpecialKey KeyF6) Down _ _) = do
  cfoldM_ (requestHaulers $ HaulTask ("Wood", 20) 5 3) 2

handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) =
  cmapM_ $ \(Position p, InteractionBox pos size, StorageSpace storage, entity) -> do
    (InfoPanel ety) <- gget @InfoPanel
    let newPos = pos - (defaultRectSizeV2 / 2)
    if isInsideInteractionBox (V2 x y) (InteractionBox newPos size)
      then do
        set global $ InfoPanel entity
        spawnParticles 5
      else Control.Monad.when (ety == entity) $ set global $ InfoPanel Nothing
handleEvent _ = return ()

changeIdlePoint :: Int -> Int -> System' ()
changeIdlePoint ety1 ety2 = cmapM_ $
  \(Building, Position pos, Entity e1) ->
    when (e1 == ety1) $ cmap $ 
      \(Villager _, IdlePoint ip, Entity e2) -> 
        if e2 == ety2 then IdlePoint pos
        else IdlePoint ip
