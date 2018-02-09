module Solver where

import Assignment
import Formula3CNF
import Control.Monad.Random hiding (fromListMay, fromList)
import Control.Arrow
-- import System.Random

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Foldable (minimumBy, maximumBy)

import Data.Function (on)

import qualified Debug.Trace as DEBUG

type Tries = Int
type Flips = Int
type Variable = Int
type Selector = Assignment -> Vector Clause -> Rand StdGen Variable

type SolverResult = (Assignment, Flips)

compareFlips :: SolverResult -> SolverResult -> Int
compareFlips (_, f) (_, f') = f' - f

--------------------------------------------------------------------------------
-- Generic ProbSAT -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | This generic version, as well as 'probSAT' are implemented according to
--   Balint, Adrian, and Uwe Schöning. 
--   "Choosing probability distributions for stochastic local search and the role of make versus break." 
--   International Conference on Theory and Applications of Satisfiability Testing.
--   Springer, Berlin, Heidelberg, 2012.
solver :: Selector -> StdGen -> Tries -> Flips -> [Assignment] -> Formula -> (Maybe SolverResult, StdGen)
solver selector gen tries flips supply formula = runRand (solver' selector tries flips supply 0 formula) gen

solver' :: Selector -> Tries -> Flips -> [Assignment] -> Flips -> Formula -> Rand StdGen (Maybe SolverResult)
solver' _        0        _        _      _     _                 = pure Nothing
solver' _        _        _        []     _     _                 = pure Nothing
solver' selector maxTries maxFlips (a:as) flips f@(Formula _ cls) = do
  mr <- search maxFlips a
  case mr of
    -- if the 'search' was unsuccessful, repeat
    Nothing          -> solver' selector (maxTries - 1) maxFlips as (flips + maxFlips) f
    -- otherwise, return the found assignment
    -- as well as the total number of flips needed to find it
    -- including all restarts (#restarts * maxFlips)
    Just (a',flips') -> pure $ Just $ (a', flips + (maxFlips - flips'))
 where
    search :: Flips -> Assignment -> Rand StdGen (Maybe (Assignment, Flips))
    search 0        _ = pure Nothing
    search maxFlips a 
      -- if a satisfying assignment is found, return it and the remaining flips
      | a `satisfies` f = pure $ Just (a, maxFlips)
      -- otherweise choose a literal, flip it and repeat
      | otherwise       = do
          variable <- selector a cls
          search (maxFlips - 1) (flipVar variable a)


--------------------------------------------------------------------------------
-- Normal ProbSAT --------------------------------------------------------------
--------------------------------------------------------------------------------

type Score = Vector Clause -> Assignment -> Variable -> Double

probSAT :: Score -> StdGen -> Tries -> Flips -> [Assignment] -> Formula -> (Maybe SolverResult, StdGen)
probSAT score = solver $ \assignment clauses -> do
  let unsat = Vector.filter (not . satisfies assignment) clauses
  Clause (x,y,z) <- uniform unsat
  let lits = map getLit [x,y,z]
      ss@[sx,sy,sz] = map (score clauses assignment) lits
      total = sx + sy + sz
      [px,py,pz] = map (/ total) ss
  r <- getRandomR (0,1)
  let l | r <= px    = x
        | r <= px+py = y
        | otherwise  = z
  pure (getLit l)


scorePoly :: Double  -- ^ c_m
          -> Double  -- ^ c_b
          -> Double  -- ^ eps
          -> Score
scorePoly cMake cBreak eps clauses assignment variable = (m ** cMake) / (eps + b ** cBreak)
  where
    m = makeScore  clauses assignment variable
    b = breakScore clauses assignment variable
{-# INLINE scorePoly #-}


scoreExp :: Double  -- ^ c_m
         -> Double  -- ^ c_b
         -> Double  -- ^ eps
         -> Score
scoreExp cMake cBreak eps clauses assignment variable = (cMake ** m) / (eps + cBreak ** b)
  where
    m = makeScore  clauses assignment variable
    b = breakScore clauses assignment variable
{-# INLINE scoreExp #-}


makeScore :: Vector Clause -> Assignment -> Variable -> Double
makeScore cs a v = toEnum made
  where
    made = count (\c -> satisfies (flipVar v a) c && not (satisfies a c)) cs
{-# INLINE makeScore #-}


breakScore :: Vector Clause -> Assignment -> Variable -> Double
breakScore cs a v = toEnum broken
  where
    broken = count (\c -> not (satisfies (flipVar v a) c) && satisfies a c) cs
{-# INLINE breakScore #-}

count :: (a -> Bool) -> Vector a -> Int
count p = Vector.map p' >>> Vector.sum
  where
    p' x | p x       = 1
         | otherwise = 0
--------------------------------------------------------------------------------
-- ProbSAT using Entropy -------------------------------------------------------
--------------------------------------------------------------------------------

probSATWithEntropy :: Score -> StdGen -> Tries -> Flips -> [Assignment] -> Formula -> (Maybe SolverResult, StdGen)
probSATWithEntropy score = solver $ \assignment clauses -> do
  let unsat          = Vector.filter (not . satisfies assignment) clauses
      c@(Clause (x,y,z)) = minimumBy (compare `on` entropy clauses assignment score) unsat
      lits           = map getLit [x,y,z]
      ss@[sx,sy,sz]  =  map (score clauses assignment) lits
      total          = sx + sy + sz
      [px,py,pz] = map (/ total) ss
  r <- getRandomR (0,1)
  let l | r <= px    = x
        | r <= px+py = y
        | otherwise  = z
  pure (getLit l)


log2 :: Double -> Double
log2 = logBase 2
{-# INLINE log2 #-}


entropy' :: Double -> Double -> Double -> Double
entropy' p1 p2 p3 = - p1 * log2 p1 - p2 * log2 p2 - p3 * log2 p3
{-# INLINE entropy' #-}


entropy :: Vector Clause -> Assignment -> Score -> Clause -> Double
entropy clauses assignment score (Clause (lx,ly,lz)) = entropy' (sx/total) (sy/total) (sz/total)
  where
    s@[sx,sy,sz] = map (getLit >>> score clauses assignment) [lx,ly,lz]
    total = sx + sy + sz
{-# INLINE entropy #-}
