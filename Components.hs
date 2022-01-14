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
    Position(..), TargetPosition(..), IdlePoint(..),
    Velocity(..), Kinetic(..), IdleMovement(..),
    Particle(..),
    MousePosition(..), Rng(..), GlobalUnique(..),
    Villager(..), Hauler(..), Builder(..),
    Building(..), StorageSpace(..), Backpack(..),
    BoundingBox(..), InteractionBox(..),
    DrawLevel(..), InfoPanel(..),
    EntityName(..), Origin(..), Destination(..),
    HaulTask(..), HaulRequest(..),
    Sprite(..),
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
import DataTypes (StorageList, DrawLevels (Default), EntityState, StorageItem)

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype TargetPosition = TargetPosition (V2 Float) deriving Show
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

newtype Villager = Villager EntityState deriving Show
instance Component Villager where type Storage Villager = Map Villager

data IdleMovement = IdleMovement Float Float Float deriving Show
instance Component IdleMovement where type Storage IdleMovement = Map IdleMovement

newtype IdlePoint = IdlePoint (V2 Float) deriving Show
instance Component IdlePoint where type Storage IdlePoint = Map IdlePoint

newtype Sprite = Sprite Rectangle deriving Show
instance Component Sprite where type Storage Sprite = Map Sprite

data Building = Building deriving Show
instance Component Building where type Storage Building = Map Building

data Hauler = Hauler deriving Show
instance Component Hauler where type Storage Hauler = Map Hauler

data Builder = Builder deriving Show
instance Component Builder where type Storage Builder = Map Builder

newtype StorageSpace = StorageSpace StorageList deriving Show
instance Component StorageSpace where type Storage StorageSpace = Map StorageSpace

newtype Backpack = Backpack (Maybe StorageItem) deriving Show
instance Component Backpack where type Storage Backpack = Map Backpack

data HaulTask = HaulTask StorageItem Int Int deriving Show
instance Component HaulTask where type Storage HaulTask = Map HaulTask

data BuildTask = BuildTask StorageItem Int Int deriving Show
instance Component BuildTask where type Storage BuildTask = Map BuildTask

newtype Rng = Rng StdGen deriving Show
instance Semigroup Rng where (<>) = mappend
instance Monoid Rng where mempty = Rng $ mkStdGen 1
instance Component Rng where type Storage Rng = Global Rng

data BoundingBox = BoundingBox (V2 Float) (V2 Float) deriving Show
instance Component BoundingBox where type Storage BoundingBox = Map BoundingBox

newtype DrawLevel = DrawLevel DrawLevels deriving (Show, Eq)
instance Semigroup DrawLevel where (<>) = mappend
instance Monoid DrawLevel where mempty = DrawLevel Default
instance Component DrawLevel where type Storage DrawLevel = Global DrawLevel

data InteractionBox = InteractionBox (V2 Float) (V2 Float) deriving Show
instance Component InteractionBox where type Storage InteractionBox = Map InteractionBox

newtype EntityName = EntityName String deriving Show
instance Component EntityName where type Storage EntityName = Map EntityName

newtype Origin = Origin Int deriving Show
instance Component Origin where type Storage Origin = Map Origin

newtype Destination = Destination Int deriving Show
instance Component Destination where type Storage Destination = Map Destination

newtype InfoPanel = InfoPanel (Maybe Entity) deriving Show
instance Semigroup InfoPanel where (<>) = mappend
instance Monoid InfoPanel where mempty = InfoPanel Nothing
instance Component InfoPanel where type Storage InfoPanel = Global InfoPanel

data HaulRequest = HaulRequest StorageItem Int deriving Show
instance Component HaulRequest where type Storage HaulRequest = Map HaulRequest

data ConstructRequest = ConstructRequest deriving Show
instance Component ConstructRequest where type Storage ConstructRequest = Map ConstructRequest

-- data Request = Request HaulRequest | Request ConstructRequest deriving Show
-- instance Component Request where type Storage Request = Map Request

type Kinetic = (Position, Velocity)

makeWorld "World" [
    ''Position, ''Velocity, ''Particle, ''MousePosition, ''GlobalUnique, ''Camera, ''Villager,
    ''IdleMovement, ''TargetPosition, ''IdlePoint, ''Rng, ''Sprite, ''Building, ''StorageSpace,
    ''BoundingBox, ''DrawLevel, ''InteractionBox, ''InfoPanel, ''EntityName, ''Hauler, ''Origin,
    ''Destination, '' Builder, ''Backpack, ''HaulTask, ''HaulRequest]

type System' a = System World a

initWorld' = initWorld

