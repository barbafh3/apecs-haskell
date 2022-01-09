{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications #-}
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
import DataTypes (DrawLevels(..), EntityState (Idle))
import Apecs.Physics (Collision(Collision))
import Utils (gget, translate', truncate')
import Input (handleEvent)
import Draw (draw)
import Text.Printf (printf)

windowConfig = InWindow "ApecsTest" (1280, 900) (10, 10)

initialize ::StdGen ->  System' ()
initialize rng = do
  newEntity GlobalUnique
  replicateM_ 1 $
    newEntity (
      Hauler,
      (Villager Idle,
      (StorageSpace [("Wood", 20)],
      (Backpack Nothing,
      (HaulTask Nothing Nothing Nothing,
      (BoundingBox (V2 0.0 0.0) (V2 8.0 8.0),
      (IdleMovement 20 3.0 0.0,
      (IdlePoint $ V2 0.0 0.0,
      (Position $ V2 0.0 0.0,
      (Velocity $ V2 60.0 60.0,
      (Sprite $ Rectangle (6 * tileSize, 12 * tileSize) defaultRectSize,
      TargetPosition Nothing)))))))))))
  newEntity (
      Building,
      EntityName "House",
      Sprite $ Rectangle (1 * tileSize, 2 * tileSize) defaultRectSize,
      InteractionBox (V2 100.0 20.0) defaultRectSizeV2,
      Position $ V2 100.0 20.0)
  newEntity (
      Building,
      EntityName "Idle Point",
      Sprite $ Rectangle (2 * tileSize, 6 * tileSize) defaultRectSize,
      InteractionBox (V2 0.0 0.0) defaultRectSizeV2,
      Position $ V2 0.0 0.0)
  newEntity $ Rng rng
  newEntity $ DrawLevel Default
  newEntity $ InfoPanel False ""
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
