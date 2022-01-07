{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

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
import Particles (stepParticles, spawnParticles)
import Constants
import Tilemap (createTilemap)
import Villagers (idleTick, checkIdleTimer, idleMove)

windowConfig = InWindow "ApecsTest" (1280, 900) (10, 10)

initialize ::StdGen ->  System' ()
initialize rng = do
  newEntity GlobalUnique
  newEntity (
      Villager,
      Sprite $ Rectangle (6 * 16, 12 * 16) defaultRectSize, 
      Position $ V2 0.0 0.0, 
      IdleMovement 20 3.0 0.0, 
      IdlePoint $ V2 0.0 0.0,
      Velocity $ V2 60.0 60.0, 
      TargetPosition Nothing)
  newEntity (
      Building, 
      Sprite $ Rectangle (2 * 16, 6 * 16) defaultRectSize,
      Position $ V2 0.0 0.0)
  newEntity (Rng rng)
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

draw :: Maybe Picture -> [Picture] -> System' Picture
draw mts tilemap = 
  case mts of
    Just (Bitmap ts) -> do
      particles <- foldDraw $
        \(Particle t, Velocity (V2 vx vy), Position (V2 px py)) ->
          translate px py $ color black $ circleSolid t
      villagers <- foldDraw $
        \(Villager, Sprite rect, Position pos) -> translate' (Position pos) $ BitmapSection rect ts
      buildings <- foldDraw $
        \(Building, Sprite rect, Position pos) -> translate' (Position pos) $ BitmapSection rect ts
      -- return $ Pictures tilemap <> villagers <> particles
      return $ Pictures tilemap <> buildings <> villagers <> particles
    _ -> return blank

step :: StdGen -> Float -> System' ()
step rng dt = do
  idleTick dt
  checkIdleTimer dt
  idleMove dt
  -- spawnParticles
  stepPosition dt
  stepParticles dt

handleEvent :: Event -> System' ()
handleEvent (EventMotion (x, y)) = set global $ MousePosition (V2 x y)
handleEvent _ = return ()

stepPosition :: Float -> System' ()
stepPosition dT = cmap $ \(Particle t, Position p, Velocity (V2 vx vy)) -> (Position (p + V2 vx vy), Velocity (V2 vx (vy - 0.1)))

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y
