module Solver where

import Assignment
import Formula3CNF
import Control.Monad.Random hiding (fromListMay, fromList)
-- import System.Random

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable (minimumBy)

import Data.Function (on)

-- import qualified Debug.Trace as DEBUG

type Tries = Int
type Flips = Int
type Variable = Int
type Selector = Assignment -> Set Clause -> Rand StdGen Variable

data SolverResult = SolverResult Assignment Tries Flips
  deriving Show

compareSolverResult :: SolverResult -> SolverResult -> (Int, Int)
compareSolverResult (SolverResult _ t f) (SolverResult _ t' f')
  = (t-t',f-f')

getFlips :: SolverResult -> Int
getFlips (SolverResult _ _ f) = f

getTries :: SolverResult -> Int
getTries (SolverResult _ t _) = t

--------------------------------------------------------------------------------
-- Utilities -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- From: 
--  https://hackage.haskell.org/package/MonadRandom-0.5.1/docs/src/Control-Monad-Random-Class.html#fromList

-- | Sample a random value from a weighted list.  The list must be
--   non-empty and the total weight must be non-zero.
fromList :: (MonadRandom m) => [(a, Double)] -> m a
fromList ws = do
  ma <- fromListMay ws
  case ma of
    Nothing -> error "Control.Monad.Random.Class.fromList: empty list, or total weight = 0"
    Just a  -> return a

-- | Sample a random value from a weighted list.  Return @Nothing@ if
--   the list is empty or the total weight is zero.
fromListMay :: (MonadRandom m) => [(a, Double)] -> m (Maybe a)
fromListMay xs = do
  let s    = sum (map snd xs) :: Double
      cums = scanl1 (\ ~(_,q) ~(y,s') -> (y, s'+q)) xs
  case s of
    0 -> return Nothing
    _ -> do
      p <- getRandomR (0, s)
      return . Just . fst . head . dropWhile ((< p) . snd) $ cums


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

solver' :: Selector -> Tries -> Flips -> [Assignment] -> Tries -> Formula -> Rand StdGen (Maybe SolverResult)
solver' _        0        _        _      _     _ = pure Nothing
solver' _        _        _        []     _     _ = pure Nothing
solver' selector maxTries maxFlips (a:as) tries f = do
  mr <- search maxFlips selector 0 a f
  case mr of
    Nothing         -> solver' selector (maxTries - 1) maxFlips as (tries + 1) f
    Just (a',flips) -> pure $ Just $ SolverResult a' tries flips

search :: Flips -> Selector -> Flips -> Assignment -> Formula -> Rand StdGen (Maybe (Assignment, Flips))
search 0        _        _     _ _ = pure Nothing
search maxFlips selector flips a f@(Formula _ cls)
  | a `satisfies` f = pure $ Just (a, flips)
  | otherwise       = do
      variable <- selector a cls
      search (maxFlips - 1) selector (flips + 1) (flipVar variable a) f


--------------------------------------------------------------------------------
-- Normal ProbSAT --------------------------------------------------------------
--------------------------------------------------------------------------------

type Score = Set Clause -> Assignment -> Variable -> Double

probSAT :: Score -> StdGen -> Tries -> Flips -> [Assignment] -> Formula -> (Maybe SolverResult, StdGen)
probSAT score = solver $ \assignment clauses -> do
  let unsat = Set.filter (not . satisfies assignment) clauses
  Clause (x,y,z) <- uniform unsat
  let lits = map getLit [x,y,z]
      [sx,sy,sz] = map (score clauses assignment) lits
      total = sx + sy + sz
  l <- fromList [(x,sx/total),(y,sy/total),(z,sz/total)]
  pure (getLit l)


scorePoly :: Double  -- ^ c_m
          -> Double  -- ^ c_b
          -> Double  -- ^ eps
          -> Score
scorePoly cMake cBreak eps clauses assignment variable = (m ** cMake) / (eps + b ** cBreak)
  where
    m = makeScore  clauses assignment variable
    b = breakScore clauses assignment variable

scoreExp :: Double  -- ^ c_m
         -> Double  -- ^ c_b
         -> Double  -- ^ eps
         -> Score
scoreExp cMake cBreak eps clauses assignment variable
  = (cMake ** m) / (cBreak ** b)
  where
    m = makeScore  clauses assignment variable
    b = breakScore clauses assignment variable

makeScore :: Set Clause -> Assignment -> Variable -> Double
makeScore cs a v = toEnum made
  where
    sat   = Set.filter (satisfies (flipVar v a)) cs
    unsat = Set.filter (not . satisfies a) cs
    made  = Set.size $ Set.intersection sat unsat

breakScore :: Set Clause -> Assignment -> Variable -> Double
breakScore cs a v = toEnum broken
  where
    unsat  = Set.filter (satisfies (flipVar v a)) cs
    sat    = Set.filter (not . satisfies a) cs
    broken = Set.size $ Set.intersection sat unsat

--------------------------------------------------------------------------------
-- ProbSAT using Entropy -------------------------------------------------------
--------------------------------------------------------------------------------

probSATWithEntropy :: Score -> StdGen -> Tries -> Flips -> [Assignment] -> Formula -> (Maybe SolverResult, StdGen)
probSATWithEntropy score = solver $ \assignment clauses -> do
  let unsat          = Set.filter (not . satisfies assignment) clauses
      Clause (x,y,z) = minimumBy (compare `on` entropy clauses assignment score) unsat
      lits           = map getLit [x,y,z]
      [sx,sy,sz]     = map (score clauses assignment) lits
      total          = sx + sy + sz
  l <- fromList [(x,sx/total),(y,sy/total),(z,sz/total)]
  pure (getLit l)

log2 :: Double -> Double
log2 = logBase 2

entropy' :: [Double] -> Double
entropy' = sum . map (\p -> - p * log2 p)

entropy :: Set Clause -> Assignment -> Score -> Clause -> Double
entropy clauses assignment score (Clause (lx,ly,lz)) = entropy' (map (/ total) s)
  where
    lits = map getLit [lx,ly,lz]
    s@[sx,sy,sz] = map (score clauses assignment) lits
    total = sx + sy + sz
