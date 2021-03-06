{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeApplications            #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Apecs
import Apecs.Gloss
import Apecs.Physics (Collision(Collision))
import Linear
import System.Random
import System.Exit
import Control.Monad
import Data.Semigroup (Semigroup)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss (Rectangle(Rectangle))
import Graphics.UI.GLUT (stringWidth, fontHeight)
import Graphics.UI.GLUT.Fonts
import Engine.Components
import Engine.Particles (stepParticles, spawnParticles, stepParticlePositions)
import Engine.Constants
import Engine.Tilemap (createTilemap)
import Engine.Villagers (idleTick, checkIdleTimer, updateVillagerCollisions, updateVillagers, spawnHauler)
import Engine.DataTypes (DrawLevels(..), EntityState (Idle, Carrying, Loading))
import Engine.Utils (gget, translate', truncate')
import Engine.Input (handleEvent)
import Engine.Draw (draw)
import Engine.Buildings (updateBuildings, spawnHouse, spawnStorage)

windowConfig = InWindow "ApecsTest" (1280, 900) (10, 10)

initialize ::StdGen ->  System' ()
initialize rng = do
  spawnHauler (V2 0.0 100.0) (V2 0.0 0.0) (V2 60.0 60.0)
  spawnHauler (V2 50.0 100.0) (V2 0.0 0.0) (V2 60.0 60.0)
  spawnHouse $ V2 300.0 200.0
  newEntity (
      Building,
      EntityName "Idle Point",
      Sprite $ Rectangle (2 * tileSize, 6 * tileSize) defaultRectSize,
      BoundingBox (V2 0.0 0.0) (V2 8.0 8.0),
      InteractionBox (V2 0.0 0.0) defaultRectSizeV2,
      Position $ V2 0.0 0.0)
  spawnStorage (V2 (-300.0) 50.0) [("Wood", 100)]
  newEntity (
      Button False,
      Sprite $ Rectangle (1 * tileSize, 2 * tileSize) defaultRectSize,
      InteractionBox (V2 (-500.0) (-400.0)) (defaultRectSizeV2 * 2),
      Position $ V2 (-500.0) (-400.0))
  newEntity $ Rng rng
  newEntity $ DrawLevel Debug
  newEntity $ InfoPanel Nothing
  maybeWhiteFont <- liftIO $ loadJuicyPNG whiteFontPath
  maybeBlackFont <- liftIO $ loadJuicyPNG blackFontPath
  newEntity $ WhiteFont maybeWhiteFont
  newEntity $ BlackFont maybeBlackFont
  printVillagers
  printBuildingNames

main :: IO ()
main = do
  w <- initWorld'
  seed <- randomIO
  let rng = mkStdGen seed
  maybeTileset <- loadJuicyPNG tilesetPath
  let (rng', tilemap) = createTilemap rng maybeTileset
  runWith w $ do
    initialize rng
    play windowConfig (makeColorI 116 210 102 255) targetFps (draw maybeTileset tilemap) handleEvent (step rng)

step :: StdGen -> Float -> System' ()
step rng dT = do
  drawLevel <- gget @DrawLevel
  Control.Monad.when (drawLevel == DrawLevel All || drawLevel == DrawLevel Particles) $ spawnParticles 1
  updateVillagers dT
  updateBuildings dT
  stepParticles dT
  stepParticlePositions dT

printBuildingNames :: System' ()
printBuildingNames = cmapM_ $ \(Building, EntityName name, Entity e) -> liftIO $ print $ "Name: " ++ name ++ " - Entity: " ++ show e

printVillagers :: System' ()
printVillagers = cmapM_ $ \(Villager state, Entity e) -> liftIO $ print $ "Villager - Entity: " ++ show e
