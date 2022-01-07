module Utils ((<+>), (<->), vectorLength, normalizeVector) where
import Linear (V2(V2))

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
