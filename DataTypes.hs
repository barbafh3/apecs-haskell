module DataTypes (StorageItem(..), StorageList(..), DrawLevels(..), EntityState(..)) where

type StorageItem = (String, Int)

type StorageList = [StorageItem]

data DrawLevels = Default | DrawCollision | DrawParticles | DrawAll | DrawDebug deriving (Show, Eq)

data EntityState = Idle | Carrying | Loading | Constructing | Enabled | Disabled deriving (Show, Eq)

emptyStorageList :: StorageList
emptyStorageList = [("Wood", 0)]

addToStorage :: StorageItem -> StorageList -> StorageList
addToStorage item [] = [item]
addToStorage item [pair]
    | fst pair == fst item = [(fst pair, snd pair + snd item)]
    | otherwise = [pair]
addToStorage item (pair : list)
    | fst pair == fst item = (fst pair, snd pair + snd item) : addToStorage item list
    | otherwise = addToStorage item list

removeFromStorage :: StorageItem -> StorageList -> StorageList
removeFromStorage item [] = [item]
removeFromStorage item [pair]
    | fst pair == fst item = if snd pair - snd item <= 0 
                               then [(fst pair, 0)]
                                 else [(fst pair, snd pair - snd item)]
    | otherwise = [pair]
removeFromStorage item (pair : list)
    | fst pair == fst item = if snd pair - snd item <= 0 
                               then (fst pair, 0) : addToStorage item list 
                                 else (fst pair, snd pair - snd item) : addToStorage item list 
    | otherwise = addToStorage item list

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
