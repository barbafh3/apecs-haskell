{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Villagers (
  idleTick,
  checkIdleTimer,
  moveToTarget,
  updateVillagerCollisions,
  updateVillagers,
  requestHaulers
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
import Buildings (addToStorage, removeFromStorage)
import Data.Maybe (isNothing, isJust)
import Collisions (areBoxesColliding)
import Control.Monad (when, unless)
import Data.Foldable (forM_)

updateVillagers :: Float ->  System' ()
updateVillagers dT = do
  runIdleState dT
  runCarryingState dT
  runLoadingState dT
  moveToTarget dT
  updateVillagerCollisions dT

runIdleState :: Float -> System' ()
runIdleState dT = do
  checkForHaulTask
  idleTick dT
  checkIdleTimer dT

runCarryingState :: Float -> System' ()
runCarryingState dT = do
  checkCarryDestination
  checkEmptyBackpack
  reachedDeliveryDestination

runLoadingState :: Float -> System' ()
runLoadingState dT = do
  checkPickupDestination
  checkFilledBackpack
  reachedPickupDestination

moveToTarget :: Float -> System' ()
moveToTarget dT = cmapM $
    \(Villager state, Position pos, TargetPosition tPos, Velocity (V2 vx vy), Entity e) -> do
        if vectorLength (tPos - pos) > 2.0
           then return $ Position $ pos + ((* vx) . (* dT) <$> normalizeVector (tPos - pos))
              else return $ Position pos

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
               then (IdleMovement radius baseT baseT, TargetPosition newTarget, Rng rng')
                 else (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)
              where
                (rng', newTarget) = getNewTarget rng ip radius
          _ -> (IdleMovement radius baseT idleT, TargetPosition mPos, Rng rng)

checkForHaulTask :: System' ()
checkForHaulTask = cmap $ \(Villager state, HaulTask {}) -> 
  case state of
    Idle -> Villager Loading
    _ -> Villager state

updateVillagerCollisions :: Float -> System'()
updateVillagerCollisions _ =
  cmap $ \(Villager _, Position (V2 x y), BoundingBox (V2 rx ry) size) -> BoundingBox (V2 x y) size

checkEmptyBackpack :: System' ()
checkEmptyBackpack = cmapM_ $
  \(Villager state, HaulTask {}, Backpack mItem, villager) -> case state of
      Carrying -> case mItem of
                    Just item -> return ()
                    Nothing -> set villager (Villager Idle, Nothing :: (Maybe HaulTask))
      _ ->  return ()

checkFilledBackpack :: System' ()
checkFilledBackpack = cmap $
  \(Villager state, HaulTask {}, Backpack mItem) -> case state of
      Loading -> case mItem of
                   Just item -> Villager Carrying
                   Nothing -> Villager state
      _ -> Villager state

checkCarryDestination :: System' ()
checkCarryDestination = cmapM_ $
    \(Villager state, HaulTask item orig dest, villager) -> do
      when (state == Carrying) $ do
          (Building, Position pos) <- get (Entity dest)
          (TargetPosition target) <- get villager
          unless (pos == target) do
            set villager (Villager state, TargetPosition pos)

checkPickupDestination :: System' ()
checkPickupDestination = cmapM $
    \(Villager state, HaulTask item orig dest, TargetPosition target) ->
      if state == Loading then do
                 get (Entity orig) >>= \case
                     Just (Building, Position pos) -> return (Villager state, TargetPosition pos)
                     Nothing -> return (Villager Idle, TargetPosition target)
      else return (Villager state, TargetPosition target)

reachedDeliveryDestination :: System' ()
reachedDeliveryDestination = cmapM_ $
    \(Villager state, HaulTask item orig dest, villager) -> do
        when (state == Carrying) $ do
            (buildingBox, StorageSpace storage) <- get (Entity dest)
            (Villager state, villagerBox, IdleMovement radius baseT _, Backpack bMItem) <- get villager
            when (areBoxesColliding buildingBox villagerBox) $ do
                set (Entity dest) $ StorageSpace $ addToStorage bMItem storage
                set villager (Villager Idle, Backpack Nothing, IdleMovement radius baseT 0.0, Nothing :: (Maybe HaulTask))

reachedPickupDestination :: System' ()
reachedPickupDestination = cmapM_ $
    \(Villager state, HaulTask item orig dest, villager) ->
        when (state == Loading) $ do
            (buildingBox, StorageSpace storage) <- get (Entity orig)
            (Villager state, villagerBox, Backpack _) <- get villager
            when (areBoxesColliding buildingBox villagerBox) $ do
                let newStorage = removeFromStorage (Just item) storage
                unless (storage == newStorage) do
                    set (Entity orig) $ StorageSpace newStorage
                    set villager (Backpack $ Just item)

requestHaulers :: HaulTask -> Int -> (Villager, Entity) -> System' Int
requestHaulers taskParams remainingJobs (Villager state, villager) =
  if remainingJobs == 0
    then pure 0
    else case state of
      Idle -> do
          addHaulTask taskParams villager
          pure (remainingJobs - 1)
      _ -> pure remainingJobs

addHaulTask :: HaulTask -> Entity -> System' ()
addHaulTask haul ety = modify ety $ \(Villager state) -> haul
