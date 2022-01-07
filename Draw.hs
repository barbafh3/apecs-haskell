{-# LANGUAGE TypeApplications #-}
module Draw (draw) where
import Apecs.Gloss
import Components
import Linear (V2(V2))
import Utils (gget, translate')
import DataTypes (DrawLevels(..))

draw :: Maybe Picture -> [Picture] -> System' Picture
draw mts tilemap =
  case mts of
    Just (Bitmap ts) -> do
        drawLevel <- gget @DrawLevel
        (InfoPanel infoPanelEnabled infoPanelText) <- gget @InfoPanel
        villagers <- foldDraw $
            \(Villager, Sprite rect, Position pos) -> translate' (Position pos) $ BitmapSection rect ts
        buildings <- foldDraw $
            \(Building, Sprite rect, Position pos) -> translate' (Position pos) $ BitmapSection rect ts
        particles <- case drawLevel of
                       level | level == DrawLevel DrawParticles || level == DrawLevel DrawAll -> foldDraw $
                                  \(Particle t, Velocity (V2 vx vy), Position (V2 px py)) -> translate px py $ color black $ circleSolid t
                             | otherwise -> return blank
        collision <- case drawLevel of
                       level | level == DrawLevel DrawCollision || level == DrawLevel DrawAll -> foldDraw $
                                  \(BoundingBox (V2 x y) (V2 w h)) -> translate x y $ color (makeColor 1.0 0.0 0.0 0.3) $ rectangleSolid w h
                             | otherwise -> return blank
        interaction <- case drawLevel of
                       level | level == DrawLevel DrawCollision || level == DrawLevel DrawAll -> foldDraw $
                                  \(InteractionBox (V2 x y) (V2 w h)) -> translate x y $ color (makeColor 0.0 0.0 1.0 0.3) $ rectangleSolid w h
                             | otherwise -> return blank
        ui <- if infoPanelEnabled then pure $ drawBoxWithText infoPanelText else pure blank
        return $ Pictures tilemap <> buildings <> villagers <> particles <> collision <> interaction <> ui
    _ -> return blank


drawBoxWithText :: String -> Picture
drawBoxWithText boxText = Pictures [translate 0.0 150.0 $ color (makeColor 0.0 0.0 0.0 0.3) $ rectangleSolid 100 50, translate (-10.0) 145 $ color white $ scale 0.1 0.1 $ text boxText]


