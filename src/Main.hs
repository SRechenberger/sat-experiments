module Main (main) where

import Assignment
-- import Formula3CNF
import Generator
import Solver

import System.Random

-- import Control.Monad (forM_)

-- import Data.Maybe (isJust)
-- import Data.List (intercalate)

main :: IO ()
main = do
  let n = 50
      m = 200
      t = 1000 -- 20 * div (4^n) (3^n)
  (genF, genAS) <- split <$> getStdGen
  let (genA, genS) = split genAS
      (f, _)   = generate3CNF genF n m
      s            = streamOfRandomAssignments genA n
      (r, _)       = probSAT (scoreExp 0.5 2.5 1) genS t (3*n) s f
      (r', _)      = probSATWithEntropy (scoreExp 0.5 2.5 1) genS t (3*n) s f
  print r
  print r'



