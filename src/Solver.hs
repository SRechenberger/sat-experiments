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

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Foldable (minimumBy, maximumBy)

import Data.Function (on)

import Text.Printf (printf)

import qualified Debug.Trace as DEBUG

type Tries = Int
type Flips = Int
type Variable = Int
type Selector = ScoreBoard -> Assignment -> Vector Clause -> Rand StdGen Literal

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
  mr <- search maxFlips a (makeScoreBoard f)
  case mr of
    -- if the 'search' was unsuccessful, repeat
    Nothing          -> solver' selector (maxTries - 1) maxFlips as (flips + maxFlips) f
    -- otherwise, return the found assignment
    -- as well as the total number of flips needed to find it
    -- including all restarts (#restarts * maxFlips)
    Just (a',flips') -> pure $ Just $ (a', flips + (maxFlips - flips'))
 where
    search :: Flips -> Assignment -> ScoreBoard -> Rand StdGen (Maybe (Assignment, Flips))
    search 0        _ _ = pure Nothing
    search maxFlips a sb
      -- if a satisfying assignment is found, return it and the remaining flips
      | a `satisfies` f = pure $ Just (a, maxFlips)
      -- otherweise choose a literal, flip it and repeat
      | otherwise       = do
          literal <- selector sb a cls
          search (maxFlips - 1) (flipVar (getLit literal) a) sb


--------------------------------------------------------------------------------
-- Normal ProbSAT --------------------------------------------------------------
--------------------------------------------------------------------------------

type Score = ScoreBoard -> Assignment -> Literal -> Double

probSAT :: Score -> StdGen -> Tries -> Flips -> [Assignment] -> Formula -> (Maybe SolverResult, StdGen)
probSAT score = solver $ \scoreboard assignment clauses -> do
  let unsat = Vector.filter (not . satisfies assignment) clauses
  Clause (x,y,z) <- uniform unsat
  let lits = [x,y,z]
      ss@[sx,sy,sz] = map (score scoreboard assignment) lits
      total = sx + sy + sz
      [px,py,pz] = map (/ total) ss
  r <- getRandomR (0,1)
  let l | r <= px    = x
        | r <= px+py = y
        | otherwise  = z
  pure l


scorePoly :: Double  -- ^ c_b
          -> Double  -- ^ eps
          -> Score
scorePoly cBreak eps sb assgn variable = 1 / (eps + b ** cBreak)
  where
    b = getBreakScore variable assgn sb
{-# INLINE scorePoly #-}


scoreExp :: Double  -- ^ c_b
         -> Double  -- ^ eps
         -> Score
scoreExp cBreak eps sb assgn variable = 1 / (eps + cBreak ** b)
  where
    b = getBreakScore variable assgn sb
{-# INLINE scoreExp #-}

--------------------------------------------------------------------------------
-- ProbSAT using Entropy -------------------------------------------------------
--------------------------------------------------------------------------------

probSATWithEntropy :: Score -> StdGen -> Tries -> Flips -> [Assignment] -> Formula -> (Maybe SolverResult, StdGen)
probSATWithEntropy score = solver $ \scoreboard assignment clauses -> do
  let unsat          = Vector.filter (not . satisfies assignment) clauses
      c@(Clause (x,y,z)) = minimumBy (compare `on` entropy scoreboard assignment score) unsat
      lits           = [x,y,z]
      ss@[sx,sy,sz]  = map (score scoreboard assignment) lits
      total          = sx + sy + sz
      [px,py,pz] = map (/ total) ss
  r <- getRandomR (0,1)
  let l | r <= px    = x
        | r <= px+py = y
        | otherwise  = z
  pure l


log2 :: Double -> Double
log2 = logBase 2
{-# INLINE log2 #-}


entropy' :: Double -> Double -> Double -> Double
entropy' p1 p2 p3 = - p1 * log2 p1 - p2 * log2 p2 - p3 * log2 p3
{-# INLINE entropy' #-}


entropy :: ScoreBoard -> Assignment -> Score -> Clause -> Double
entropy sb assgn score (Clause (lx,ly,lz)) = entropy' (sx/total) (sy/total) (sz/total)
  where
    s@[sx,sy,sz] = map (score sb assgn) [lx,ly,lz]
    total = sx + sy + sz
{-# INLINE entropy #-}

--------------------------------------------------------------------------------
-- Scoreboard ------------------------------------------------------------------
--------------------------------------------------------------------------------

type ScoreBoard = Map Variable BoardLine

data BoardLine = BoardLine
  { positives :: Vector Clause
  , negatives :: Vector Clause
  -- , breaks    :: !Int
  }
  deriving Show

makeScoreBoard :: Formula -> ScoreBoard
makeScoreBoard (Formula varCnt cs) = Map.fromList
  [(i, makeBoardLine cs i) | i <- [0..varCnt-1]]

makeBoardLine :: Vector Clause -> Int -> BoardLine
makeBoardLine clauses var = BoardLine
  { positives = poss
  , negatives = negs
--  , breaks    = if testVar var assgn
--    then count (\c -> satisfiedLiterals assgn c == 1) poss
--    else count (\c -> satisfiedLiterals assgn c == 1) negs
  }

 where
  poss = Vector.filter (containsLiteral (Pos var)) clauses
  negs = Vector.filter (containsLiteral (Neg var)) clauses


getBreakScore :: Literal -> Assignment -> ScoreBoard -> Double
getBreakScore lit assgn sb = toEnum $ case Map.lookup (getLit lit) sb of
  Nothing -> error $ printf "getBreakScore: missing variable: %d" (getLit lit)
  Just ln
    | testVar (getLit lit) assgn -> count (\c -> satisfiedLiterals assgn c == 1) (positives ln)
    | otherwise              -> count (\c -> satisfiedLiterals assgn c == 1) (negatives ln)
{-
getBreakScore' :: Int -> ScoreBoard -> Double
getBreakScore' i sb = case Map.lookup i sb of
  Nothing -> error $ printf "getBreakScore': missing variable: %d" i
  Just sb -> toEnum $ breaks sb

updateBreakScore' :: Literal -> Assignment -> ScoreBoard -> ScoreBoard
updateBreakScore' lit assgn sb = case Map.lookup (getLit lit) sb of
  Nothing -> error $ printf "updateBreakScore: missing variable: %d" (getLit lit)
  Just ln -> let (ln', updates) = updateBreakScore' assgn lit ln
                 sb' = Map.insert (getLit lit) ln' sb
              in foldl (\sb (i,v) -> Map.adjust (\bl -> bl { breaks = v + breaks bl} ) i sb) sb' updates


updateBreakScore' :: Assignment -> Literal -> BoardLine -> (BoardLine, [(Variable, Int)])
updateBreakScore' assgn lit bl = (bl', updates)
 where
  bl' = bl
    { breaks = if isPos lit
      then count (\c -> satisfiedLiterals assgn c == 0) (negatives bl)
      else count (\c -> satisfiedLiterals assgn c == 0) (positives bl)
    }
  updates = Map.toList updatesPos ++ Map.toList (Map.map negate updatesNeg)
  updatesPos'
    | testVar (getLit lit) assgn = positives bl
    | otherwise                  = negatives bl
  updatesPos = Map.delete (getLit lit) $ countOccs updatesPos'
  updatesNeg'
    | testVar (getLit lit) assgn = negatives bl
    | otherwise                  = positives bl
  updatesNeg = Map.delete (getLit lit) $ countOccs updatesNeg'

countOccs :: Vector Clause -> Map Variable Int
countOccs = Vector.foldl u (Map.empty)
  where
    u :: Map Variable Int -> Clause -> Map Variable Int
    u m c = foldl (\m' i -> Map.alter u' i m') m (map getLit $ clauseToList c)

    u' Nothing  = Just 1
    u' (Just x) = Just $ x + 1
-}
count :: (a -> Bool) -> Vector a -> Int
count p = Vector.map p' >>> Vector.sum
  where
    p' x | p x       = 1
         | otherwise = 0
