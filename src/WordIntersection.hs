{-# LANGUAGE BangPatterns #-}
module WordIntersection
       (
         Letter (..)
       , TheWord

       , intersectionNumber

       , stringToReducedWord
       , isReduced
       , word
       , randWord
       , isCyclicallyReduced

       , randReducedWord
       , randIntersectionNumber
       )
       where

import Data.List (subsequences, nub, intersperse)
import Control.Applicative
import Control.Monad
import Control.DeepSeq
import MonteCarlo
--import Control.Parallel.Strategies


data Letter = A | A' | B | B' deriving (Eq, Ord, Enum, Bounded)
instance NFData Letter where
  rnf x = x `seq` ()

newtype TheWord = TheWord {letters :: [Letter]}

word :: [Letter] -> TheWord
word = TheWord

isReduced :: [Letter] -> Bool
isReduced (_:[]) = True
isReduced (x:y:xs) | x == inverse y = False
                   | otherwise = isReduced (y:xs)

isCyclicallyReduced :: TheWord -> Bool
isCyclicallyReduced w = isReduced xs && not (head xs == inverse(last xs))
  where xs = letters w

stringToWord :: String -> TheWord
stringToWord = word . map fromChar

stringToReducedWord :: String -> TheWord
stringToReducedWord xs = let ys = stringToWord xs in
  if isCyclicallyReduced ys then ys else error "Unreduced word"

fromChar :: Char -> Letter
fromChar 'a' = A
fromChar 'A' = A'
fromChar 'b' = B
fromChar 'B' = B'
fromChar c = error (c:" is not a valid letter! Use only a, A, b, or B")

inverse :: Letter -> Letter
inverse A = A'
inverse B = B'
inverse A' = A
inverse B' = B

omicron :: [Letter] -> Int
omicron w
       | isNotUnique w  =  0
       | isCyclicSubset w =  1
       | isCyclicSubset $ reverse w = -1
       | otherwise = 0
       where
         isCyclicSubset w = any ((w `elem`) . subsequences) $ take 4 $ cycles [A, A', B, B']
         isNotUnique w = (length . nub) w /= length w


inverse' :: [Letter] -> [Letter]
inverse' = map inverse . reverse

cycles :: [a] -> [[a]]
cycles   = iterate (\x -> last x:init x)

mu :: Int -> [Letter] -> [Letter] -> Int
mu k c d
       | length c < k || length d <k = 0
       | c1 == d1 || ck == dk || ci /= di  = 0
       | k == 2 && omicron[inverse c1, inverse d1, ckm1, dkm1] /= 0   = 1
       | k >= 3 && omicron[inverse c1, inverse d1, c2] == omicron[ck, dk, inverse ckm1] = 1
       | otherwise = 0
       where
        (cK, dK) = (take k c, take k d)
        (c1, d1) = (head cK, head dK)
        (ck, dk) = (last cK, last dK)
        (ci, di) = (init . tail  $ cK, init . tail  $ dK)
        (c2, d2) = if k >= 3 then (head ci, head di) else (last ci, last di)
        (ckm1, dkm1) = if k >= 3 then (last ci, last di) else (last cK, last dK)

upsilon :: Int -> [Letter] -> [Letter] -> Int
upsilon k c d
       | k == 2 = 0
       | k >= 3 && length c >= k && length d >= k  = mu k ctk $ inverse' dtk
       | otherwise = 0
       where (ctk, dtk) = (take k c, take k d)

intersectionNumber :: TheWord -> Int
intersectionNumber w' = sum [mu k (sigma i w) (sigma j w) + upsilon k (sigma i w) (sigma j w) | i <- [1..n], j <- [i+1..n], k <- [2..n]]
              where
                w = letters w'
                n     = length w
                sigma i w = cycles w !!i



randLetter :: Rand Letter
randLetter = (toEnum . (`mod`4)) <$> randInt

randWord :: Int -> Rand TheWord
randWord n = word <$> replicateM n randLetter

randReducedWord :: Int -> Rand TheWord
randReducedWord n = do w <- randWord n
                       if isCyclicallyReduced w
                          then return w
                          else randReducedWord n

randIntersectionNumber :: Int -> Rand Int
randIntersectionNumber n = intersectionNumber <$> randReducedWord n


{-
randCycRedWords :: Int -> PureMT -> [TheWord]
randCycRedWords n s = filter isCyclicallyReduced $ evalState (sequence $ repeat $ randWord n) s


randomLetter :: PureMT -> (Letter, PureMT)
randomLetter s = (toEnum (n `mod` 4), s')
   where (n, s') = randomInt s

-}


instance Show Letter where
  show A = "a"
  show B = "b"
  show A' = "a'"
  show B' = "b'"

instance Show TheWord where
  show = concat . intersperse "" . map show . letters
