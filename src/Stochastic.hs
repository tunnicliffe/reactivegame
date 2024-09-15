{-# LANGUAGE BangPatterns #-}

module Stochastic 
  ( splitN
  , brownianMotion2D 
  , brownianMotion3D
  ) where

import FRP.Yampa
import System.Random

import Data.Bits ((.&.))
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32)
import Control.Parallel.Strategies (parMap, rseq)

import SDL (V2 (..), V3 (..))


split3 :: (RandomGen g) => g -> (g, g, g)
split3 g = (g1, g2, g3) where
  (g0, g1) = split g 
  (g2, g3) = split g0

splitN :: (RandomGen g) => Int -> g -> [g]
splitN n g | n < 1 = error "splitN: n < 1."
splitN 1 g = [g]
splitN n g = let (g', g'') = split g in g' : splitN (n-1) g''

-- Sampling functions adapted from 
-- https://hackage.haskell.org/package/mwc-random-0.14.0.0/docs/src/System.Random.MWC.Distributions.html

-- Unboxed 2-tuple
data T = T {-# UNPACK #-} !Double {-# UNPACK #-} !Double

sampleNormal :: (RandomGen g)
       => Double                -- Mean
       -> Double                -- Standard deviation
       -> g
       -> (Double, g)
{-# INLINE sampleNormal #-}
sampleNormal m s g0 = (m + s * x, g1) where (x, g1) = sampleStandardNormal g0

-- | Generate a normally distributed random variate with zero mean and
-- unit variance.
--
-- The implementation uses Doornik's modified ziggurat algorithm.
-- Compared to the ziggurat algorithm usually used, this is slower,
-- but generates more independent variates that pass stringent tests
-- of randomness.
sampleStandardNormal :: (RandomGen g) => g -> (Double, g)
{-# INLINE sampleStandardNormal #-}
sampleStandardNormal g0 = 
  let
    (u0, g1) = random g0
    u = 2 * u0 - 1
    (ri, g2) = random g1
    i = fromIntegral ((ri :: Word32) .&. 127)
    bi = VU.unsafeIndex blocks i 
    bj = VU.unsafeIndex blocks (i+1)
  in 
    if abs u < VU.unsafeIndex ratios i
    then (u * bi, g2)
    else if i == 0
      then normalTail (u < 0) g2
      else let
        x  = u * bi 
        xx = x * x 
        d  = exp (-0.5 * (bi * bi - xx))
        e  = exp (-0.5 * (bj * bj - xx))
        (c, g3) = random g2
      in 
        if e + c * (d - e) < 1
        then (x, g3)
        else sampleStandardNormal g3

normalTail :: (RandomGen g) => Bool -> g -> (Double, g)
normalTail neg g0 =
  let
    (u1, g1) = random g0
    (u2, g2) = random g1
    x = log u1 / rNorm 
    y = log u2
  in 
    if y * (-2) < x * x 
      then normalTail neg g2
      else 
        if neg
          then (x - rNorm, g2)
          else (rNorm - x, g2)

blocks :: VU.Vector Double
blocks = (`VU.snoc` 0) . VU.cons (v/f) . VU.cons rNorm . VU.unfoldrN 126 go $! T rNorm f
  where
    go (T b g) = let !u = T h (exp (-0.5 * h * h))
                     h  = sqrt (-2 * log (v / b + g))
                 in Just (h, u)
    v = 9.91256303526217e-3
    f = exp (-0.5 * rNorm * rNorm)
{-# NOINLINE blocks #-}

rNorm :: Double
rNorm = 3.442619855899

ratios :: VU.Vector Double
ratios = VU.zipWith (/) (VU.tail blocks) blocks
{-# NOINLINE ratios #-}

---

sampleN :: (RandomGen g) => Int -> (g -> (a, g)) -> g -> ([a], g)
sampleN n f g =
  if n >= 0
    then sampleNInternal n f g
    else error $ "sampleN given negative n = " ++ show n
sampleNInternal 0 _ g0 = ([], g0)
sampleNInternal n f g0 = (x : xs, g2)
  where
  (x, g1) = f g0
  (xs, g2) = sampleNInternal (n - 1) f g1

parSample :: (RandomGen g) => (g -> (a, g)) -> [g] -> [(a, g)]
parSample = parMap rseq

parSampleN :: (RandomGen g) => Int -> (g -> (a, g)) -> [g] -> [([a], g)]
parSampleN n f = parMap rseq (sampleN n f)

---

-- iterFrom :: (a -> a -> DTime -> b -> b) -> b -> SF a b

randomProcess
  :: (RandomGen g)
  => ((a, [g]) -> (a, [g]) -> DTime -> (b, [g]) -> (b, [g]))  -- IterFrom update function
  -> b                                                        -- Output initialiser value. Never actually returned by the stream function
  -> [g]
  -> SF a b
randomProcess update b0 g0 = loopPre g0 $ iterFrom update (b0, g0) 
  -- Input type 'a' can be used to repeatedly re-parameterise the update function, as in the example below
  -- For discrete time Markov Processes, recommend using (Event a) as the input type (Equivalent to Maybe a)

-- Example of the above in action:
-- A 2d brownian motion with variable standard deviation
brownianMotion2DVar
  :: (RandomGen g)
  => V2 Double              -- Initial position
  -> (g, g)
  -> SF Double (V2 Double)  -- Standard deviation as input, position as output
brownianMotion2DVar pos0 (ga, gb) = randomProcess update pos0 [ga, gb] 
  where
  update _ (s, [gx0, gy0]) dt (pos, _) =
    let [(dx, gx1), (dy, gy1)] = parSample sampleStandardNormal [gx0, gy0]
    in (pos + V2 (s * dt * dx) (s * dt * dy), [gx1, gy1])

brownianMotion2D 
  :: (RandomGen g)
  => V2 Double
  -> Double
  -> g
  -> g
  -> SF () (V2 Double)
brownianMotion2D pos0 sd ga gb = randomProcess update pos0 [ga, gb] 
  where
  update _ (_, [gx0, gy0]) dt (pos, _) =
    let [(dx, gx1), (dy, gy1)] = parSample sampleStandardNormal [gx0, gy0]
    in (pos + V2 (sd * dt * dx) (sd * dt * dy), [gx1, gy1])

brownianMotion3D 
  :: (RandomGen g)
  => V3 Double
  -> Double
  -> g
  -> g
  -> g
  -> SF () (V3 Double)
brownianMotion3D pos0 sd ga gb gc = randomProcess update pos0 [ga, gb, gc] 
  where
  update _ (_, [gx0, gy0, gz0]) dt (pos, _) =
    let [(dx, gx1), (dy, gy1), (dz, gz1)] = parSample sampleStandardNormal [gx0, gy0, gz0]
    in (pos + V3 (sd * dt * dx) (sd * dt * dy) (sd * dt * dz), [gx1, gy1, gz1])

-- Provide a steam of samples from a fixed distribution
sampleStream
  :: (RandomGen g)
  => (g -> (a, g))  -- Fixed distribution to sample
  -> a              -- initial output
  -> g 
  -> SF () a 
sampleStream f a0 g0 = loopPre g0 (iterFrom sampleIter (a0, g0)) 
  where sampleIter _ (_, g) _ _ = f g 

-- Provide a stream of samples from a distribution given its parameters as input
sampleStreamParam
  :: (RandomGen g)
  => (a -> g -> (b, g))   -- Parameterised distribution to sample
  -> b                    -- initial output
  -> g
  -> SF a b
sampleStreamParam f b0 g0 = loopPre g0 (iterFrom sampleIterParam (b0, g0)) 
  where sampleIterParam _ (a, g) _ _ = f a g

-- Want: Something of the form SF (Event a) b for modelling Discrete time Markov Processes

-- Want: A way to convert Q and P matricies to IterFrom update functions,
-- so we can then go from Q or P matricies direct to simulation SFs
