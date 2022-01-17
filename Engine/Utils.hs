{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Engine.Utils ((<+>), (<->), vectorLength, normalizeVector, gget, translate', truncate') where
    
import Linear (V2(V2))
import Apecs.Core
import Apecs.Gloss
import Engine.Components (Position (Position))
import Apecs (get, global)

sumV2 :: V2 Float -> Float
sumV2 (V2 x y) = x + y

-- | Adds both vectors
(<+>) :: V2 Float -> V2 Float -> V2 Float
(V2 x1 y1) <+> (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

-- | Subtracts the second vector form the first vector
(<->) :: V2 Float -> V2 Float -> V2 Float
(V2 x1 y1) <-> (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

vectorLength :: V2 Float -> Float
vectorLength vec = sqrt . sumV2 $ (** 2) <$> vec

normalizeVector :: V2 Float -> V2 Float
normalizeVector vec = (/ vectorLength vec) <$> vec

gget :: forall c w m . (Has w m c, Apecs.Core.ExplGet m (Storage c)) => SystemT w m c 
gget = get global

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

truncate' decimals number = let
    toFloat n  = read n :: Float
    totalChars = (+) (decimals+1) $ getPos '.' (show number) 0
        where getPos c (x:xs) n
                  | x == c    = n
                  | otherwise = getPos c xs n+1
    in toFloat $ take totalChars $ show number
