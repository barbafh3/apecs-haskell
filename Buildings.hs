module Buildings (
    emptyStorageList, 
    addToStorage, 
    removeFromStorage, 
    totalUsage, 
    resourceCount
) where

import Components
import DataTypes

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

removeFromStorage :: StorageItem -> StorageList -> StorageList
removeFromStorage item [] = [item]
removeFromStorage item [pair]
    | fst pair == fst item = if snd pair - snd item <= 0 
                               then [(fst pair, 0)]
                                 else [(fst pair, snd pair - snd item)]
    | otherwise = [pair]
removeFromStorage item (pair : list)
    | fst pair == fst item = if snd pair - snd item <= 0 
                               then (fst pair, 0) : removeFromStorage item list 
                                 else (fst pair, snd pair - snd item) : removeFromStorage item list 
    | otherwise = removeFromStorage item list

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



