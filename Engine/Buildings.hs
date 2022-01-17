{-# LANGUAGE TypeApplications           #-}

module Engine.Buildings (
  updateBuildings,
  emptyStorageList,
  addToStorage,
  removeFromStorage,
  totalUsage,
  resourceCount,
  requestHaulers,
  spawnHouse
) where

import Engine.Components
import Engine.DataTypes
import Apecs
import Engine.Constants (haulerCapacity, tileSize, defaultRectSize, defaultRectSizeV2)
import Engine.Utils (truncate')
import Linear (V2(..))
import Graphics.Gloss (Rectangle(Rectangle))

spawnHouse :: V2 Float -> System' ()
spawnHouse pos = do
  newEntity (
      Building,
      EntityName "House",
      HaulRequest ("Wood", 60) 0,
      Sprite $ Rectangle (1 * tileSize, 2 * tileSize) defaultRectSize,
      StorageSpace [],
      BoundingBox pos (V2 8.0 8.0),
      InteractionBox pos defaultRectSizeV2,
      Position pos)
  return ()

updateBuildings :: Float -> System'()
updateBuildings dT = do
  runBuildingTask

emptyStorageList :: StorageList
emptyStorageList = [("Wood", 0)]

addToStorage :: Maybe StorageItem -> StorageList -> StorageList
addToStorage Nothing [] = []
addToStorage (Just item) [] = [item]
addToStorage (Just item) [pair]
    | fst pair == fst item = [(fst pair, snd pair + snd item)]
    | otherwise = [pair]
addToStorage (Just item) (pair : list)
    | fst pair == fst item = (fst pair, snd pair + snd item) : addToStorage (Just item) list
    | otherwise = addToStorage (Just item) list
addToStorage Nothing (pair : list) = pair : list

removeFromStorage :: Maybe StorageItem -> StorageList -> StorageList
removeFromStorage Nothing [] = []
removeFromStorage (Just item) [] = [item]
removeFromStorage (Just item) [pair]
    | fst pair == fst item = if snd pair - snd item <= 0
                               then [(fst pair, 0)]
                                 else [(fst pair, snd pair - snd item)]
    | otherwise = [pair]
removeFromStorage (Just item) (pair : list)
    | fst pair == fst item = if snd pair - snd item <= 0
                               then (fst pair, 0) : removeFromStorage (Just item) list
                                 else (fst pair, snd pair - snd item) : removeFromStorage (Just item) list
    | otherwise = removeFromStorage (Just item) list
removeFromStorage Nothing (pair : list) = pair : list

totalUsage :: StorageList -> Int
totalUsage [] = 0
totalUsage [pair] = snd pair
totalUsage (pair : list) = snd pair + totalUsage list

resourceCount :: String -> StorageList -> Int
resourceCount _ [] = 0
resourceCount resource [pair]
    | fst pair == resource = snd pair
    | otherwise = 0
resourceCount resource (pair : list)
    | fst pair == resource = snd pair + resourceCount resource list
    | otherwise = resourceCount resource list


runBuildingTask :: System' ()
runBuildingTask = cmapM_ $
  \(Building, HaulRequest (resource, requiredamount) amount, building) -> do
    let haulerCount = round $ (fromIntegral requiredamount :: Float) / haulerCapacity
    let (Entity ety) = building
    cfoldM_ (requestHaulers (HaulTask (resource, ceiling haulerCapacity) 5 ety) building) haulerCount

requestHaulers :: HaulTask -> Entity -> Int -> (Villager, Entity) -> System' Int
requestHaulers haul building remainingSpots (Villager state, villager) =
  if remainingSpots == 0
    then pure 0
    else case state of
      Idle -> do
        set villager haul
        (HaulRequest (resource, requiredAmount) amount) <- get building
        let new_amount = round $ fromIntegral amount + haulerCapacity
        set building $
          if new_amount >= requiredAmount then
            Right $ Not @HaulRequest
          else
            Left $ HaulRequest (resource,  requiredAmount) new_amount
        pure (remainingSpots - 1)
      _ -> pure remainingSpots


