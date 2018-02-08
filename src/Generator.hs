--------------------------------------------------------------------------------
-- Generator for Random 3CNF Formulas; implemented according to
--
-- Uwe Schöning and Jacobo Torán.
-- The Satisfiability Problem: Algorithms and Analyses.
-- Vol. 3. Lehmanns Media, 2013.
-- Chapter 7.2
--------------------------------------------------------------------------------

module Generator where

import Assignment
import Formula3CNF

import System.Random

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (sort)


generate3CNF :: StdGen -> Int -> Int -> (Formula, StdGen)
generate3CNF gen n m = (Formula n f, gen'')
  where
    (a, gen')  = randomAssignment gen n
    (f, gen'') = generate3CNF' gen n m a Set.empty

streamOfRandom3CNF :: StdGen -> Int -> Int -> [Formula]
streamOfRandom3CNF gen n m = f : streamOfRandom3CNF gen' n m
  where
    (f, gen') = generate3CNF gen n m

generate3CNF' :: StdGen -> Int -> Int -> Assignment -> Set Clause -> (Set Clause, StdGen)
generate3CNF' gen _ 0 _ acc = (acc, gen) 
generate3CNF' gen n m a acc
  | c `Set.member` acc = generate3CNF' gen'' n m a acc
  | otherwise = generate3CNF' gen'' n (m-1) a (Set.insert c acc)
  where
    ((x,y,z), gen') = chooseVars gen n
    (r, gen'') = randomR (0.0,1.0) gen'
    c = choose r [ (cl, clauseProb a cl) | l1 <- [Pos x, Neg x], l2 <- [Pos y, Neg y], l3 <- [Pos z, Neg z], let cl = Clause (l1,l2,l3), a `satisfies` cl ]

choose :: Double -> [(a, Double)] -> a
choose = choose' 0

choose' :: Double -> Double -> [(a,Double)] -> a
choose' _    _ []         = error $ "choose: empty list."
choose' pAcc r ((x,p):xs) | r <= pAcc + p = x
                          | otherwise     = choose' (pAcc+p) r xs

chooseVars :: StdGen -> Int -> ((Int,Int,Int), StdGen)
chooseVars gen n = ((x,y,z), gen')
  where
    (l, gen') = genKUnequal gen 3 []
    [x,y,z] = sort l

    genKUnequal gen 0 acc = (acc, gen)
    genKUnequal gen k acc 
      | r `elem` acc = genKUnequal g k acc
      | otherwise    = genKUnequal g (k-1) (r:acc)
      where
        (r,g) = randomR (0,n-1) gen
        

clauseProb :: Assignment -> Clause -> Double
clauseProb a clause = case satisfiedLiterals a clause of
  1 -> 0.191
  2 -> 0.118
  3 -> 0.073 
