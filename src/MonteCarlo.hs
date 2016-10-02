{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module MonteCarlo
       (
         Rand
       , RandSeed
       , newRandSeed

       , randInt
       , random
       , randoms

       , randFinite
       , randCountable
       , randBool
       , randToss
       , headCount
       , tailCount

       , sample
       , monteCarloAvg
       , monteCarloFiniteIntegrate
       , monteCarloIntegrate
       , Coin
       ) where

import System.Random.Mersenne.Pure64 (PureMT, randomInt, newPureMT)
import Control.Monad.State.Lazy (State, state, evalState, replicateM)
import Data.List (foldl')

type Rand a = State PureMT a
type RandSeed = PureMT


sample :: Int -> Rand a -> RandSeed -> [a]
sample n f = evalState (replicateM n f)

newRandSeed :: IO RandSeed
newRandSeed = newPureMT

randInt :: Rand Int
randInt = state randomInt



{-# SPECIALIZE INLINE randFinite :: forall a . (Enum a, Bounded a) => Rand a #-}
randFinite :: forall a . (Enum a, Bounded a) => Rand a
randFinite = toEnum . (`mod` k) <$> randInt
  where
    k = fromEnum (maxBound :: a) + 1

randCountable :: Enum a => Rand a
randCountable = toEnum <$> randInt

randBool :: Rand Bool
randBool = randFinite


data Coin = Head | Tail deriving (Show, Eq, Ord, Enum, Bounded)

headCount Head = 1
headCount otherwise = 0

tailCount Tail = 1
tailCount otherwise = 0

randToss :: Rand Coin
randToss = randFinite

monteCarloAvg :: (Real n, Fractional m) => Int -> Rand a -> (a -> n) -> RandSeed -> m
monteCarloAvg n r f s = avg $ sample n ((realToFrac . f) <$> r) s
  where
    avg :: (Real a, Fractional b) => [a] -> b
    avg xs = realToFrac s' / realToFrac n'
      where (s', n') = foldl' (\ !(!s, !n) x -> (s+x, n+1)) (0, 0) xs

monteCarloFiniteIntegrate n f = monteCarloAvg n randFinite f

monteCarloIntegrate n f = monteCarloAvg n randVal f

class Randomizable a where
  randVal :: Rand a

instance Randomizable Coin where randVal = randFinite
instance Randomizable Bool where randVal = randFinite

random :: Randomizable a => RandSeed -> a
random = evalState randVal

randoms :: Randomizable a => RandSeed -> [a]
randoms = evalState (sequence $ repeat randVal)
