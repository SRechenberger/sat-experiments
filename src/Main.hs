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

  let t = 20

  let n = 100
  let m = 427

  let fs = streamOfRandom3CNF genF n m
      as = streamOfRandomAssignments genA n

  let results = testFormulas genF t (fromEnum $ 100*n) n as fs
  putStr
    $ show
    $ length
    $ filter id
    $ map (\(b, s, s') -> s > s')
    $ results
  putStrLn $ " of " ++ show t


testFormulas :: StdGen -> Int -> Int -> Int -> [Assignment] -> [Formula] -> [(Bool, Stat, Stat)]
testFormulas _ 0 _ _ _ _ = []
testFormulas gen n maxTries maxFlips as (f:fs) = (isJust a',s,s') : testFormulas gen' (n-1) maxTries maxFlips as fs
  where
    (a ,gen',s) = probSAT gen maxTries maxFlips as f (Stat 0 0)
    (a',_  ,s') = entropySAT gen maxTries maxFlips as f (Stat 0 0)
testFormulas _ _ _ _ _ _ = error "Fuck"
