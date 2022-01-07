module Villagers (idleTick, checkIdleTimer, idleMove) where
import Components (System', IdleMovement (IdleMovement), TargetPosition (TargetPosition), IdlePoint (IdlePoint), Position (Position), Velocity (Velocity), Rng (Rng))
import System.Random
import Apecs.Physics (V2(V2))
import Apecs
import Utils (vectorLength, normalizeVector)
import Debug.Trace (trace)


idleTick :: Float -> System' ()
idleTick dT = cmap $ \(IdleMovement radius baseT idleT) -> 
    case () of
        _ | idleT > 0 -> IdleMovement radius baseT (idleT - dT)
          | otherwise -> IdleMovement radius baseT idleT

idleMove :: Float -> System' ()
idleMove dT = cmap $ \(Position pos, TargetPosition tPos, Velocity (V2 vx vy)) -> 
  case tPos of
     Just target@(V2 tx ty) -> if vectorLength (target - pos) > 2.0
                                 then Position $ pos + ((* vx) . (* dT) <$> normalizeVector (target - pos))
                                   else Position pos
     Nothing -> Position pos

getNewTarget :: StdGen -> V2 Float -> Float -> (StdGen, V2 Float)
getNewTarget rng (V2 px py) radius = trace ("New target: " ++ show (V2 nx ny)) (rng'', V2 nx ny)
    where
        (minX, minY) = (px - radius, py - radius)
        (maxX, maxY) = (px + radius, py + radius)
        (nx, rng') = uniformR (minX, maxX) rng
        (ny, rng'') = uniformR (minY, maxY) rng'

checkIdleTimer :: Float -> System' ()
checkIdleTimer dt = cmap checkTimer
      where 
        checkTimer (IdleMovement radius baseT idleT, IdlePoint ip, TargetPosition mPos, Rng rng) = if idleT <= 0.0
           then (IdleMovement radius baseT baseT, TargetPosition $ Just newTarget, Rng rng')
             else (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)
          where
            (rng', newTarget) = getNewTarget rng ip radius
