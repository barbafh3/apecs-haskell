{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Components(
    Position(..), 
    TargetPosition(..), 
    Velocity(..), 
    Particle(..), 
    MousePosition(..), 
    GlobalUnique(..), 
    Kinetic(..), 
    Villager(..),
    IdleMovement(..),
    IdlePoint(..),
    Rng(..),
    Sprite(..),
    Building(..),
    System',
    World,
    initWorld'
) where

import Apecs
import Apecs.Gloss
import Linear
import System.Random
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss (Rectangle(Rectangle))

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype TargetPosition = TargetPosition (Maybe (V2 Float)) deriving Show
instance Component TargetPosition where type Storage TargetPosition = Map TargetPosition

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

newtype Particle = Particle Float deriving Show
instance Component Particle where type Storage Particle = Map Particle

newtype MousePosition = MousePosition (V2 Float) deriving Show
instance Semigroup MousePosition where (<>) = mappend
instance Monoid MousePosition where mempty = MousePosition (V2 0.0 0.0)
instance Component MousePosition where type Storage MousePosition = Global MousePosition

data GlobalUnique = GlobalUnique deriving Show
instance Component GlobalUnique where type Storage GlobalUnique = Unique GlobalUnique

data Villager = Villager deriving Show
instance Component Villager where type Storage Villager = Map Villager

data IdleMovement = IdleMovement Float Float Float deriving Show
instance Component IdleMovement where type Storage IdleMovement = Map IdleMovement

newtype IdlePoint = IdlePoint (V2 Float) deriving Show
instance Component IdlePoint where type Storage IdlePoint = Map IdlePoint

newtype Sprite = Sprite Rectangle deriving Show
instance Component Sprite where type Storage Sprite = Map Sprite

data Building = Building deriving Show
instance Component Building where type Storage Building = Map Building

newtype Rng = Rng StdGen deriving Show
instance Semigroup Rng where (<>) = mappend
instance Monoid Rng where mempty = Rng $ mkStdGen 1
instance Component Rng where type Storage Rng = Global Rng

type Kinetic = (Position, Velocity)


makeWorld "World" [
    ''Position, ''Velocity, ''Particle, ''MousePosition, ''GlobalUnique, ''Camera, ''Villager,
    ''IdleMovement, ''TargetPosition, ''IdlePoint, ''Rng, ''Sprite, ''Building]

type System' a = System World a

initWorld' = initWorld 

