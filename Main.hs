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
import Linear
import System.Random
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss (Rectangle(Rectangle))
import Components
import Particles (stepParticles, spawnParticles, stepParticlePositions)
import Constants
import Tilemap (createTilemap)
import Villagers (idleTick, checkIdleTimer, updateVillagerCollisions, updateVillagers)
import DataTypes (DrawLevels(..), EntityState (Idle, Carrying, Loading))
import Apecs.Physics (Collision(Collision))
import Utils (gget, translate', truncate')
import Input (handleEvent)
import Draw (draw)
import Graphics.UI.GLUT (stringWidth, fontHeight)
import Graphics.UI.GLUT.Fonts

windowConfig = InWindow "ApecsTest" (1280, 900) (10, 10)

initialize ::StdGen ->  System' ()
initialize rng = do
  newEntity GlobalUnique
  replicateM_ 2 $
    newEntity (
      Hauler,
      (Villager Idle,
      (Backpack Nothing,
      (BoundingBox (V2 0.0 100.0) (V2 8.0 8.0),
      (IdleMovement 20 3.0 0.0,
      (IdlePoint $ V2 0.0 0.0,
      (Position $ V2 0.0 100.0,
      (Velocity $ V2 60.0 60.0,
      (Sprite $ Rectangle (6 * tileSize, 12 * tileSize) defaultRectSize,
      TargetPosition (V2 0.0 0.0))))))))))
  newEntity (
      Building,
      EntityName "House",
      Sprite $ Rectangle (1 * tileSize, 2 * tileSize) defaultRectSize,
      StorageSpace [("Wood", 20)],
      BoundingBox (V2 300.0 200.0) (V2 8.0 8.0),
      InteractionBox (V2 300.0 200.0) defaultRectSizeV2,
      Position $ V2 300.0 200.0)
  newEntity (
      Building,
      EntityName "Idle Point",
      Sprite $ Rectangle (2 * tileSize, 6 * tileSize) defaultRectSize,
      BoundingBox (V2 0.0 0.0) (V2 8.0 8.0),
      InteractionBox (V2 0.0 0.0) defaultRectSizeV2,
      Position $ V2 0.0 0.0)
  newEntity (
      Building,
      EntityName "Storage",
      Sprite $ Rectangle (6 * tileSize, 4 * tileSize) defaultRectSize,
      BoundingBox (V2 (-300.0) 50.0) (V2 8.0 8.0),
      InteractionBox (V2 (-300.0) 50.0) defaultRectSizeV2,
      StorageSpace [("Wood", 100)],
      Position $ V2 (-300.0) 50.0)
  newEntity $ Rng rng
  newEntity $ DrawLevel Default
  newEntity $ InfoPanel Nothing
  printVillagers
  printBuildingNames
  return ()

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
  Control.Monad.when (drawLevel == DrawLevel DrawAll || drawLevel == DrawLevel DrawParticles) $ spawnParticles 1
  updateVillagers dT
  stepParticles dT
  stepParticlePositions dT

printBuildingNames :: System' ()
printBuildingNames = cmapM_ $ \(Building, EntityName name, Entity e) -> liftIO $ print $ "Name: " ++ name ++ " - Entity: " ++ show e

printVillagers :: System' ()
printVillagers = cmapM_ $ \(Villager state, Entity e) -> liftIO $ print $ "Villager - Entity: " ++ show e
