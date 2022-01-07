module Collisions (isInsideBoundingBox, isInsideInteractionBox) where
import Linear (V2 (V2))
import Graphics.Gloss (Rectangle (Rectangle))
import Components (BoundingBox (BoundingBox), InteractionBox (InteractionBox))


isInsideBoundingBox :: V2 Float -> BoundingBox -> Bool
isInsideBoundingBox (V2 x y) (BoundingBox (V2 bx by) (V2 bw bh)) 
    | bx <= x && x <= (bx + bw) && by <= y && y <= (by + bh) = True
    | otherwise = False

isInsideInteractionBox :: V2 Float -> InteractionBox -> Bool
isInsideInteractionBox (V2 x y) (InteractionBox (V2 bx by) (V2 bw bh)) 
    | bx <= x && x <= (bx + bw) && by <= y && y <= (by + bh) = True
    | otherwise = False


areBoxesColliding :: BoundingBox -> BoundingBox -> Bool
areBoxesColliding (BoundingBox (V2 x1 y1) (V2 w1 h1)) (BoundingBox (V2 x2 y2) (V2 w2 h2))
    | x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2 = True
    | otherwise = False
