{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications #-}

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
import Villagers (idleTick, checkIdleTimer, idleMove, updateVillagerCollisions)
import DataTypes (DrawLevels(..))
import Apecs.Physics (Collision(Collision))
import Utils (gget, translate')
import Input (handleEvent)
import Draw (draw)

windowConfig = InWindow "ApecsTest" (1280, 900) (10, 10)

initialize ::StdGen ->  System' ()
initialize rng = do
  newEntity GlobalUnique
  replicateM_ 0 $ do
    newEntity (
        Villager,
        (Sprite $ Rectangle (6 * tileSize, 12 * tileSize) defaultRectSize,
        (Position $ V2 0.0 0.0,
        BoundingBox (V2 0.0 0.0) (V2 8.0 8.0),
        IdleMovement 20 3.0 0.0,
        IdlePoint $ V2 0.0 0.0,
        Velocity $ V2 60.0 60.0,
        StorageSpace [("Wood", 20)],
        TargetPosition Nothing)))
  newEntity (
      Building,
      EntityName "Idle Point",
      Sprite $ Rectangle (2 * tileSize, 6 * tileSize) defaultRectSize,
      InteractionBox (V2 0.0 0.0) defaultRectSizeV2,
      Position $ V2 0.0 0.0)
  newEntity (
      Building,
      EntityName "House",
      Sprite $ Rectangle (1 * tileSize, 2 * tileSize) defaultRectSize,
      InteractionBox (V2 100.0 20.0) defaultRectSizeV2,
      Position $ V2 100.0 20.0)
  newEntity $ Rng rng
  newEntity $ DrawLevel Default
  newEntity $ InfoPanel False ""
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
    play windowConfig white 60 (draw maybeTileset tilemap) handleEvent (step rng)

step :: StdGen -> Float -> System' ()
step rng dt = do
  drawLevel <- gget @DrawLevel
  case drawLevel of
    level | level == DrawLevel DrawAll && level == DrawLevel DrawParticles -> spawnParticles
          | otherwise -> return ()
  idleTick dt
  checkIdleTimer dt
  idleMove dt
  stepParticles dt
  stepParticlePositions dt
  updateVillagerCollisions dt
