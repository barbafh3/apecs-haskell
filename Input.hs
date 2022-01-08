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

handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) =
  cmapM_ $ \(Position p, InteractionBox pos size, EntityName name, Entity e) -> do
    let newPos = pos - (defaultRectSizeV2 / 2)
    if isInsideInteractionBox (V2 x y) (InteractionBox newPos size)
       then set global $ InfoPanel True name
          else set global $ InfoPanel False ""

handleEvent _ = return ()

