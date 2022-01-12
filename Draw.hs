{-# LANGUAGE TypeApplications #-}

module Draw (draw) where
import Apecs.Gloss
import Components
import Linear (V2(V2))
import Utils (gget, translate')
import DataTypes (DrawLevels(..))
import Apecs (Entity)
import Control.Monad.IO.Class (liftIO)
import Apecs.System (get)
import Apecs.Core (Entity(Entity))

draw :: Maybe Picture -> Picture -> System' Picture
draw mts tilemap =
  case mts of
    Just (Bitmap ts) -> do
        drawLevel <- gget @DrawLevel
        (InfoPanel infoPanelEntity) <- gget @InfoPanel
        (Villager state, Backpack bp) <- get (Entity 1)
        (EntityName n2, StorageSpace st2) <- get (Entity 3)
        (EntityName n3, StorageSpace st3) <- get (Entity 5)
        villagers <- foldDraw $
            \(Villager _, Sprite rect, Position pos) -> translate' (Position pos) $ BitmapSection rect ts
        buildings <- foldDraw $
            \(Building, Sprite rect, Position pos) -> translate' (Position pos) $ BitmapSection rect ts
        particles <- foldDraw $
            \(Particle t, Velocity (V2 vx vy), Position (V2 px py)) -> translate px py $ color (makeColorI 100 60 2 200) $ circleSolid t
        collision <- case drawLevel of
                       level | level == DrawLevel DrawCollision || level == DrawLevel DrawAll -> foldDraw $
                                  \(BoundingBox (V2 x y) (V2 w h)) -> translate x y $ color (makeColor 1.0 0.0 0.0 0.3) $ rectangleSolid w h
                             | otherwise -> return blank
        interaction <- case drawLevel of
                       level | level == DrawLevel DrawCollision || level == DrawLevel DrawAll -> foldDraw $
                                  \(InteractionBox (V2 x y) (V2 w h)) -> translate x y $ color (makeColor 0.0 0.0 1.0 0.3) $ rectangleSolid w h
                             | otherwise -> return blank
        let ui = Pictures [translate 0.0 380 $ color black $ scale 0.1 0.1 $ text (n2 ++ " - Storage: " ++ show st2),
                           translate 0.0 400.0 $ color black $ scale 0.1 0.1 $ text ("Villager state: " ++ show state ++ " - Backpack: " ++ show bp),
                           translate 0.0 360 $ color black $ scale 0.1 0.1 $ text (n3 ++ " - Storage: " ++ show st3)]
        return $ Pictures [] <> buildings <> villagers <> particles <> collision <> interaction <> ui
    _ -> return blank
