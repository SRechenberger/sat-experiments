module Main (main) where

import Assignment
import Formula3CNF
import Generator
import Solver

import System.Random

import Control.Monad (forM_)

import Data.Maybe (isJust)
import Data.List (intercalate)

main :: IO ()
main = do
  (genF,genA) <- split <$> getStdGen

  let n = 100
  let m = 400

  let fs = streamOfRandom3CNF genF n m
      as = streamOfRandomAssignments genA n

  let results = testFormulas genF 20 10 (20*(4 * m) `div` 3) as fs
  putStrLn
    $ show
    $ length
    $ filter id
    $ map (\(_, s, s') -> s > s')
    $ results


testFormulas :: StdGen -> Int -> Int -> Int -> [Assignment] -> [Formula] -> [(Bool, Stat, Stat)]
testFormulas _ 0 _ _ _ _ = []
testFormulas gen n maxTries maxFlips as (f:fs) = (isJust a && isJust a',s,s') : testFormulas gen' (n-1) maxTries maxFlips as fs
  where
    (a ,gen',s) = probSAT gen maxTries maxFlips as f (Stat 0 0)
    (a',_  ,s') = entropySAT gen maxTries maxFlips as f (Stat 0 0)
testFormulas _ _ _ _ _ _ = error "Fuck"
