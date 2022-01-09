{-# LANGUAGE TypeApplications #-}
module Input(handleEvent) where
import Components
import DataTypes (DrawLevels(..))
import Apecs
import Linear (V2(V2))
import Utils (gget)
import Collisions (isInsideInteractionBox)
import qualified Control.Monad
import Apecs.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game
import Apecs.Physics.Gloss
import Constants (defaultRectSizeV2)
import Control.Monad (when)
import Particles (spawnParticles)

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

handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) =
  cmapM_ $ \(Position p, InteractionBox pos size, EntityName name, Entity e) -> do
    (InfoPanel _ text) <- gget @InfoPanel
    let newPos = pos - (defaultRectSizeV2 / 2)
    if isInsideInteractionBox (V2 x y) (InteractionBox newPos size)
        then do
              set global $ InfoPanel True name
              spawnParticles 5
            else Control.Monad.when (text == name) $ set global $ InfoPanel False ""

handleEvent _ = return ()

changeIdlePoint :: Int -> Int -> System' ()
changeIdlePoint ety1 ety2 = cmapM_ $
  \(Building, Position pos, Entity e1) ->
    Control.Monad.when (e1 == ety1) $ cmap $ \(Villager _, IdlePoint ip, Entity e2) -> if e2 == ety2 then IdlePoint pos else IdlePoint ip
