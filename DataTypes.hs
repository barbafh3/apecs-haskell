module DataTypes (StorageItem(..), StorageList(..), DrawLevels(..), EntityState(..)) where

type StorageItem = (String, Int)

type StorageList = [StorageItem]

data DrawLevels = Default | DrawCollision | DrawParticles | DrawAll | DrawDebug deriving (Show, Eq)

data EntityState = Idle | Carrying | Loading | Constructing | Enabled | Disabled deriving (Show, Eq)

