{-# LANGUAGE TypeApplications #-}

module Engine.Draw (draw) where
import Apecs.Gloss
import Engine.Components
import Linear (V2(V2))
import Engine.Utils (gget, translate')
import Engine.DataTypes (DrawLevels(..), EntityState, StorageItem)
import Apecs (Entity)
import Control.Monad.IO.Class (liftIO)
import Apecs.System (get)
import Apecs.Core (Entity(Entity))
import Graphics.Gloss (BitmapData, Rectangle (Rectangle))
import Engine.Text

draw :: Maybe Picture -> Picture -> System' Picture
draw mts tilemap =
  case mts of
    Just ts'@(Bitmap ts) -> do
      mBFont <- gget @BlackFont
      case mBFont of
        BlackFont (Just bFont) -> do
          drawLevel <- gget @DrawLevel
          (InfoPanel infoPanelEntity) <- gget @InfoPanel
          (Villager state, Backpack bp) <- get (Entity 0)
          (EntityName n2, StorageSpace st2) <- get (Entity 4)
          (EntityName n3, StorageSpace st3, request) <- get (Entity 2)
          villagers <- foldDraw $
              \(Villager _, Sprite rect, Position pos) -> translate' (Position pos) $ BitmapSection rect ts
          buildings <- foldDraw $
              \(Building, Sprite rect, Position pos) -> translate' (Position pos) $ BitmapSection rect ts
          particles <- foldDraw $
              \(Particle t, Velocity (V2 vx vy), Position (V2 px py)) -> translate px py $ color (makeColorI 100 60 2 200) $ circleSolid t
          collision <- case drawLevel of
                         level | level == DrawLevel Collision || level == DrawLevel All -> foldDraw $
                                    \(BoundingBox (V2 x y) (V2 w h)) -> translate x y $ color (makeColor 1.0 0.0 0.0 0.3) $ rectangleSolid w h
                               | otherwise -> return blank
          interaction <- case drawLevel of
                         level | level == DrawLevel Collision || level == DrawLevel All -> foldDraw $
                                    \(InteractionBox (V2 x y) (V2 w h)) -> translate x y $ color (makeColor 0.0 0.0 1.0 0.3) $ rectangleSolid w h
                               | otherwise -> return blank
          ui <- drawUI ts bFont drawLevel
          let debugUI = case drawLevel of 
                          level | level == DrawLevel Debug || level == DrawLevel All -> drawDebugUI n2 st2 n3 st3 state request bp bFont
                                | otherwise -> blank
          return $ Pictures [] <> buildings <> villagers <> particles <> ui <> collision <> interaction <> debugUI
        _ -> return blank
    _ -> return blank

drawDebugUI :: String -> [StorageItem] -> String -> [StorageItem] -> EntityState -> Maybe HaulRequest -> Maybe StorageItem -> Picture -> Picture
drawDebugUI n2 st2 n3 st3 state request bp font = Pictures [
 drawText (V2 (-600.0) 380) font $ filterText (n2 ++ " - Storage: " ++ show st2),
 drawText (V2 (-600.0) 400) font $ filterText ("Villager state: " ++ show state ++ " - Backpack: " ++ show bp),
 drawText (V2 (-600.0) 360) font $ filterText (n3 ++ " - Storage: " ++ show st3 ++ " - Request: " ++ show (request :: Maybe HaulRequest))]

drawUI :: BitmapData -> Picture -> DrawLevel -> System' Picture
drawUI ts font (DrawLevel level) = foldDraw $ 
  \(Button clicked, Position (V2 x y), Sprite rect@(Rectangle (rx, ry) (rw, rh))) -> do 
    Pictures [
            translate x y $ color (getButtonBGColor clicked) $ scale 2.0 2.0 $ rectangleSolid (4.0 + fromIntegral rw) (4.0 + fromIntegral rh),
            translate x y $ color (makeColor 1.0 1.0 1.0 0.2) $ scale 2.0 2.0 $ rectangleSolid (fromIntegral rw) (fromIntegral rh),
            translate x y $ color white $ scale 2.0 2.0 $ BitmapSection rect ts,
            drawText (V2 (-600.0) 440) font $ filterText ("Draw level: " ++ show level)]

getButtonBGColor :: Bool -> Color
getButtonBGColor clicked = if clicked then makeColor 0.0 0.0 0.0 0.8 else makeColor 0.0 0.0 0.0 0.6
