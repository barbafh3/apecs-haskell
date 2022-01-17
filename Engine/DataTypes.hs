module Engine.DataTypes (StorageItem(..), StorageList(..), DrawLevels(..), EntityState(..)) where

type StorageItem = (String, Int)

type StorageList = [StorageItem]

data DrawLevels = Default | Collision | Particles | All | Debug deriving (Show, Eq)

data EntityState = Idle | Carrying | Loading | Constructing | Enabled | Disabled deriving (Show, Eq)

