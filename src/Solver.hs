module Solver where

import Assignment
import Formula3CNF
import Control.Monad.Random
import System.Random

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable (minimumBy, maximumBy)

import Data.Function (on)

import qualified Debug.Trace as DEBUG

data Stat = Stat Int Int
  deriving (Eq, Ord)

instance Show Stat where
  show (Stat r f) = "(" ++ show r ++ "," ++ show f ++ ")"

--------------------------------------------------------------------------------
-- 'Old' Solver ----------------------------------------------------------------
--------------------------------------------------------------------------------

probSAT :: StdGen -> Int -> Int -> [Assignment] -> Formula -> Stat -> (Maybe Assignment, StdGen, Stat)
probSAT gen 0        _        _      _ s = (Nothing, gen, s)
probSAT gen maxTries maxFlips (a:as) f s@(Stat r _) = case search gen maxFlips a f 0 of
  (Nothing, gen', _) -> probSAT gen' (maxTries - 1) maxFlips as f (Stat (r+1) 0)
  (Just a, gen', fl) -> (Just a, gen', Stat r fl)


search :: StdGen -> Int -> Assignment -> Formula -> Int -> (Maybe Assignment, StdGen, Int)
search gen 0        _ _ fl = (Nothing, gen, fl)
search gen maxFlips a f@(Formula _ cls) fl
  | a `satisfies` f = (Just a, gen, fl)
  | otherwise       = search gen'' (maxFlips - 1) (flipVar l a) f (fl + 1)
  where
    unsat = Set.filter (not . satisfies a) cls
    (r, gen') = randomR (0,Set.size unsat - 1) gen
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
    sat = Set.filter (satisfies a) cls
    unsat = Set.filter (not . satisfies (flipVar i a)) cls
    broken = Set.intersection sat unsat
    break = toEnum $ Set.size broken

    cb = 2.5

--------------------------------------------------------------------------------
-- 'New' Solver ----------------------------------------------------------------
--------------------------------------------------------------------------------


entropySAT :: StdGen -> Int -> Int -> [Assignment] -> Formula -> Stat -> (Maybe Assignment, StdGen, Stat)
entropySAT gen 0        _        _      _ s = (Nothing, gen, s)
entropySAT gen maxTries maxFlips (a:as) f s@(Stat t 0) = case search' gen maxFlips a f 0 of
  (Nothing, gen', _) -> entropySAT gen' (maxTries - 1) maxFlips as f (Stat t 0)
  (Just a, gen', fl)  -> (Just a, gen', Stat t fl)


search' :: StdGen -> Int -> Assignment -> Formula -> Int -> (Maybe Assignment, StdGen, Int)
search' gen 0        _ _ fl = (Nothing, gen, fl)
search' gen maxFlips a f@(Formula _ cls) fl
  | a `satisfies` f = (Just a, gen, fl)
  | otherwise       = -- DEBUG.trace
      -- ("Clause: " ++ show c ++ "\n" ++ "Entropy: " ++ show (entropy a f c) ++ "\n" ++ "p: " ++ show p ++ "\nSum: " ++ (show $ p1 + p2 + p3))
      (search' gen' (maxFlips - 1) (flipVar l a) f (fl+1))
  where
    unsat = Set.filter (not . satisfies a) cls
    c@(Clause (l1,l2,l3)) = minimumBy (compare `on` entropy a f) unsat
    totalScore = score (getLit l1) a f + score (getLit l2) a f + score (getLit l3) a f
    p@(p1,p2,p3) =
      ( score (getLit l1) a f / totalScore
      , score (getLit l2) a f / totalScore
      , score (getLit l3) a f / totalScore)
    (l, gen') = distro gen
      [ (getLit l1, p1)
      , (getLit l2, p2)
      , (getLit l3, p3) ]

log2 :: Double -> Double
log2 = logBase 2

entropy :: Assignment -> Formula -> Clause -> Double
entropy a f (Clause (l1,l2,l3)) = -p1 * log2 p1 - p2 * log2 p2 - p3 * log2 p3
  where
    s1 = score (getLit l1) a f
    s2 = score (getLit l2) a f
    s3 = score (getLit l3) a f
    total = s1 + s2 + s3
    p1 = s1 / total
    p2 = s2 / total
    p3 = s3 / total

entropy' :: Assignment -> Formula -> Clause -> Double
entropy' a f (Clause (l1,l2,l3)) = -p1 * log2 p1 - p2 * log2 p2 - p3 * log2 p3
  where
    s1 = score (getLit l1) a f
    s2 = score (getLit l2) a f
    s3 = score (getLit l3) a f
    total = s1 + s2 + s3
    p1 = (s1 / total)^2
    p2 = (s2 / total)^2
    p3 = (s3 / total)^2
