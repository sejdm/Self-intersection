module Main (main) where

import Prelude hiding (fmap)
import WordIntersection
import Histogram
import System.Environment (getArgs)
import System.Random.Mersenne.Pure64
import Control.Monad hiding (fmap)
import Control.Parallel.Strategies
import MonteCarlo
import Data.Monoid

main = do
  c:l:n:_ <- map read <$> getArgs
  ss <- replicateM c newPureMT

  let histie = (mconcat ((map (histogram id . sample (n `div` c) (randIntersectionNumber l)) ss) `using` parList rdeepseq))

  histToChart "test.png" histie
  putStrLn $ "Mean: " ++ show (histMean histie)
  putStrLn $ "Standard dev: " ++ show (histStdDev histie)




    --(combineHists (( map (histogram id . take (n `div` c) . map intersectionNumber . randCycRedWords l) ss) `using` parList rdeepseq))
    --((histogram id . take (read n) . map intersectionNumber . randCycRedWords (read l)) (head ss) )
