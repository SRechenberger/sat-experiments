module Solver where

import Assignment
import Formula3CNF
import Control.Monad.Random
import System.Random

import Data.Set (Set)
import qualified Data.Set as Set

probSAT :: StdGen -> Int -> Int -> [Assignment] -> Formula -> (Maybe Assignment, StdGen)
probSAT gen 0        _        _      _ = (Nothing, gen)
probSAT gen maxTries maxFlips (a:as) f = case search gen maxFlips a f of
  (Nothing, gen') -> probSAT gen' (maxTries - 1) maxFlips as f
  (Just a, gen')  -> (Just a, gen')


search :: StdGen -> Int -> Assignment -> Formula -> (Maybe Assignment, StdGen)
search gen 0        _ _ = (Nothing, gen)
search gen maxFlips a f@(Formula _ cls)
  | a `satisfies` f = (Just a, gen)
  | otherwise       = search gen'' (maxFlips - 1) (flipVar l a) f

  where
    unsat = Set.filter (not . satisfies a) cls
    (r, gen') = randomR (0,Set.size unsat) gen
    Clause (l1,l2,l3) = Set.elemAt r unsat
    totalScore = score (getLit l1) a f + score (getLit l2) a f + score (getLit l3) a f
    (l, gen'') = distro gen'
      [ (getLit l1, score (getLit l1) a f / totalScore)
      , (getLit l2, score (getLit l2) a f / totalScore)
      , (getLit l3, score (getLit l3) a f / totalScore) ]

distro :: StdGen -> [(a, Double)] -> (a, StdGen)
distro gen ds = (findGood (prepare 0 ds), gen')
  where
    (r, gen') = random gen

    prepare _ [] = []
    prepare acc ((x,p):ps) = (x,p+acc) : prepare (acc + p) ps

    findGood [] = error "fuck this"
    findGood ((x,p):ps)
      | r < p     = x
      | otherwise = findGood ps

score :: Int -> Assignment -> Formula -> Double
score i a (Formula _ cls) = cb ** (-break)
  where
    unsat = Set.filter (not . satisfies a) cls
    unsat' = Set.filter (not . satisfies (flipVar i a)) cls
    break = toEnum $ Set.size unsat' - Set.size unsat

    cb = 2.5
