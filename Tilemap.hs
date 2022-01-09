module Tilemap(createTilemap) where

import Graphics.Gloss
import System.Random
import Constants
import System.Random.Stateful (uniformM)
import Control.Applicative

createTilemap :: StdGen -> Maybe Picture -> (StdGen, Picture)
createTilemap rng (Just ts) = (rng', Pictures tiles)
  where
    (rng', tiles) = iterateOverCoords rng ts (combineIntoTuple [0..horizontalTileCount + 1] [0..verticalTileCount])
createTilemap rng Nothing = (rng, blank)

iterateOverCoords :: StdGen -> Picture -> [(Float, Float)] -> (StdGen, [Picture])
iterateOverCoords rng (Bitmap ts) [] = (rng, [])
iterateOverCoords rng (Bitmap ts) [(x, y)] = (rng', [translate (x * 16 - halfScreenX) (y * 16 - halfScreenY) $ BitmapSection (Rectangle (rx * 16, 0) (16, 16)) ts])
  where
      (rx, rng') = uniformR (0, 3) rng
      halfScreenX = fromIntegral screenWidth / 2.0
      halfScreenY = fromIntegral screenHeight / 2.0
iterateOverCoords rng (Bitmap ts) ((x, y) : coords) = (rng', translate (x * 16 - halfScreenX) (y * 16 - halfScreenY) (BitmapSection (Rectangle (rx * 16, 0) (16, 16)) ts) : snd (iterateOverCoords rng' (Bitmap ts) coords))
  where
      (rx, rng') = uniformR (0, 3) rng
      halfScreenX = fromIntegral screenWidth / 2.0
      halfScreenY = fromIntegral screenHeight / 2.0
iterateOverCoords rng _ _ = (rng, [])


combineIntoTuple :: [a] -> [a] -> [(a, a)]
combineIntoTuple = liftA2 (,)
