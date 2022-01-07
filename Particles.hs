{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Particles(spawnParticles, stepParticles) where

import Components
import Linear
import Apecs
import System.Random
import Control.Monad
import qualified Apecs.Core

spawnParticles :: System' ()
spawnParticles =
  cmapM $ \(GlobalUnique, MousePosition (V2 x y)) -> replicateM_ 1 $ do
    vx <- liftIO $ (/ 10.0) <$> randomRIO (0.0, 20.0)
    vy <- liftIO $ randomRIO (2.0, 3.0)
    t  <- liftIO $ randomRIO (4.0, 6.0)
    newEntity (Particle t, Position (V2 x y), Velocity (V2 (vx - 1.0) vy))

stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
  if t < 0
     then Right $ Not @(Particle, Kinetic)
     else Left  $ Particle (t - 0.1)


gget :: forall c w m . (Has w m c, Apecs.Core.ExplGet m (Storage c)) => SystemT w m c 
gget = Apecs.get global
