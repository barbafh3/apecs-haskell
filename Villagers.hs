{-# LANGUAGE BlockArguments #-}
module Villagers (
  idleTick,
  checkIdleTimer,
  moveToTarget,
  updateVillagerCollisions,
  updateVillagers
) where
import Components
import System.Random
import Apecs.Physics (V2(V2))
import Apecs
import Utils (vectorLength, normalizeVector)
import Debug.Trace (trace)
import Graphics.Gloss (Rectangle(Rectangle))
import DataTypes(EntityState(..))
import qualified Control.Monad
import Buildings (addToStorage)
import Data.Maybe (isNothing, isJust)
import Collisions (areBoxesColliding)
import Control.Monad (when)
import Data.Foldable (forM_)

updateVillagers :: Float ->  System' ()
updateVillagers dT = do
  runIdleState dT
  runCarryingState dT
  moveToTarget dT
  updateVillagerCollisions dT

runIdleState :: Float -> System' ()
runIdleState dT = do
  idleTick dT
  checkIdleTimer dT

runCarryingState :: Float -> System' ()
runCarryingState dT = do
  checkCarryDestination
  checkEmptyBackpack
  reachedDestination

runLoadingState :: Float -> System' ()
runLoadingState dT =
  checkFilledBackpack

moveToTarget :: Float -> System' ()
moveToTarget dT = cmap $
    \(Villager state, Position pos, TargetPosition tPos, Velocity (V2 vx vy), Entity e) ->
        case tPos of
            Just target@(V2 tx ty) -> if vectorLength (target - pos) > 2.0
                                           then Position $ pos + ((* vx) . (* dT) <$> normalizeVector (target - pos))
                                             else Position pos
            Nothing -> Position pos

idleTick :: Float -> System' ()
idleTick dT = cmap $ \(Villager state, IdleMovement radius baseT idleT) ->
    case state of
        Idle -> if idleT > 0
                     then IdleMovement radius baseT (idleT - dT)
                       else IdleMovement radius baseT idleT
        _ -> IdleMovement radius baseT idleT

getNewTarget :: StdGen -> V2 Float -> Float -> (StdGen, V2 Float)
getNewTarget rng (V2 px py) radius = (rng'', V2 nx ny)
    where
        (minX, minY) = (px - radius, py - radius)
        (maxX, maxY) = (px + radius, py + radius)
        (nx, rng') = uniformR (minX, maxX) rng
        (ny, rng'') = uniformR (minY, maxY) rng'

checkIdleTimer :: Float -> System' ()
checkIdleTimer dt = cmap checkTimer
      where
        checkTimer (Villager state, IdleMovement radius baseT idleT, IdlePoint ip, TargetPosition mPos, Rng rng) = case state of
          Idle -> if idleT <= 0.0
               then (IdleMovement radius baseT baseT, TargetPosition $ Just newTarget, Rng rng')
                 else (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)
              where
                (rng', newTarget) = getNewTarget rng ip radius
          _ -> (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)

updateVillagerCollisions :: Float -> System'()
updateVillagerCollisions _ =
  cmap $ \(Villager _, Position (V2 x y), BoundingBox (V2 rx ry) size) -> BoundingBox (V2 x y) size

checkEmptyBackpack :: System' ()
checkEmptyBackpack = cmap $
  \(Villager state, Backpack mItem) -> case state of
      Carrying -> case mItem of
                    Just item -> Villager state
                    Nothing -> Villager Idle
      _ -> Villager state

checkFilledBackpack :: System' ()
checkFilledBackpack = cmap $
  \(Villager state, Backpack mItem) -> case state of
      Loading -> case mItem of
                   Just item -> Villager Carrying
                   Nothing -> Villager state
      _ -> Villager state

checkCarryDestination :: System' ()
checkCarryDestination = cmapM $
    \(Villager state, HaulTask mItem mOrig mDest, TargetPosition mTarget) ->
      case mTarget of
         target | isNothing target && state == Carrying ->
                       case mDest of
                           Nothing -> return (Villager state, TargetPosition mTarget)
                           Just dest -> do
                               (Building, Position pos) <- get (Entity dest)
                               return (Villager state, TargetPosition $ Just pos)
                | otherwise -> return (Villager state, TargetPosition mTarget)

reachedDestination :: System' ()
reachedDestination = cmapM_ $
    \(Villager state, HaulTask mItem mOrig mDest, villager) -> 
        forM_ mDest \building -> do
            (buildingBox, StorageSpace storage) <- get (Entity building)
            (Villager state, villagerBox, Backpack bMItem) <- get villager
            when (state == Carrying && areBoxesColliding buildingBox villagerBox) $ do
                set (Entity building) $ StorageSpace $ addToStorage bMItem storage
                set villager (Backpack Nothing, Nothing :: (Maybe HaulTask))
