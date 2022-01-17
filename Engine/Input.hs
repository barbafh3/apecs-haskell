{-# LANGUAGE TypeApplications #-}

module Engine.Input (handleEvent) where

import Apecs
import Apecs.Gloss
import Apecs.Physics.Gloss
import Engine.Collisions (isInsideInteractionBox)
import Engine.Components
import Engine.Constants (defaultRectSizeV2)
import Control.Monad (when)
import qualified Control.Monad
import Engine.DataTypes (DrawLevels (..), EntityState (Idle))
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Interact
import Linear (V2 (V2))
import Engine.Particles (spawnParticles)
import Engine.Utils (gget)
import Engine.Buildings (requestHaulers)

handleEvent :: Event -> System' ()
handleEvent (EventMotion (x, y)) = set global $ MousePosition (V2 x y)

handleEvent (EventKey (SpecialKey KeyF10) Down _ _) = do
  drawLevel <- gget @DrawLevel
  case drawLevel of
    DrawLevel Particles -> set global $ DrawLevel Default
    _ -> set global $ DrawLevel Particles

handleEvent (EventKey (SpecialKey KeyF11) Down _ _) = do
  drawLevel <- gget @DrawLevel
  case drawLevel of
    DrawLevel Collision -> set global $ DrawLevel Default
    _ -> set global $ DrawLevel Collision

handleEvent (EventKey (SpecialKey KeyF9) Down _ _) = do
  drawLevel <- gget @DrawLevel
  case drawLevel of
    DrawLevel All -> set global $ DrawLevel Default
    _ -> set global $ DrawLevel All

handleEvent (EventKey (SpecialKey KeyF8) Down _ _) = do
  drawLevel <- gget @DrawLevel
  case drawLevel of
    DrawLevel Debug -> set global $ DrawLevel Default
    _ -> set global $ DrawLevel Debug

handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) = do
  cmapM_ $ 
    \(Position p, InteractionBox pos size, StorageSpace storage, entity) -> do
      (InfoPanel ety) <- gget @InfoPanel
      let newPos = pos - (defaultRectSizeV2 / 2)
      if isInsideInteractionBox (V2 x y) (InteractionBox newPos size)
        then do
          set global $ InfoPanel entity
          spawnParticles 5
        else Control.Monad.when (ety == entity) $ set global $ InfoPanel Nothing
  cmapM $ \(Button clicked, Position pos, InteractionBox _ size) -> do 
    let newPos = pos - defaultRectSizeV2
    if isInsideInteractionBox (V2 x y) (InteractionBox newPos size) 
      then return $ Button True
        else return $ Button False

handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) = cmap $ \(Button clicked) -> Button False

handleEvent _ = return ()

changeIdlePoint :: Int -> Int -> System' ()
changeIdlePoint ety1 ety2 = cmapM_ $
  \(Building, Position pos, Entity e1) ->
    when (e1 == ety1) $ cmap $ 
      \(Villager _, IdlePoint ip, Entity e2) -> 
        if e2 == ety2 then IdlePoint pos
        else IdlePoint ip
