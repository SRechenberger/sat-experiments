module Main (main) where

import Assignment
import Formula3CNF
import Generator
import Solver

import System.Random

-- import Control.Monad (forM_)

-- import Data.Maybe (isJust)
-- import Data.List (intercalate)

gamma :: Double
gamma = 4.27

n, m, t :: Int
n = 20
m = fromEnum $ toEnum n * gamma
t = 20 * fromEnum ((4/3)^n)

main :: IO ()
main = do
  putStrLn $
    "n = " ++ show n ++ "\n" ++
    "m = " ++ show m ++ "\n" ++
    "t = " ++ show t

  genF <- getStdGen
  (a,b) <- compareAlgorithms (take 10 $ streamOfRandom3CNF genF n m) (0,0)
  let total = a + b
  putStrLn $
    "probSAT:\t" ++ show a ++ " of " ++ show total
  putStrLn $
    "probSAT+:\t" ++ show b ++ " of " ++ show total


compareAlgorithms :: [Formula] -> (Int, Int) -> IO (Int,Int)
compareAlgorithms [] acc = pure acc
compareAlgorithms ls@(f:fs) (accX, accY) = do
  putStrLn $ show (length ls) ++ " formulas left."
  (genA, genS) <- split <$> getStdGen
  let s       = streamOfRandomAssignments genA n
      (r,_)   = probSAT (scorePoly 0.5 2.5 1) genS t (3*n) s f
      (r',_)  = probSATWithEntropy (scorePoly 0.5 2.5 1) genS t (3*n) s f
  acc <- case (r,r') of
    (Just r, Just r') -> do
      let a = (getTries r, getFlips r)
          b = (getTries r', getFlips r')
      if a >= b
        then pure (accX + 1, accY)
        else pure (accX, accY + 1)
    (Just r, Nothing) -> do
      putStrLn "!>"
      pure (accX + 1, accY)
    (Nothing, Just r) -> do
      putStrLn "<!"
      pure (accX, accY + 1)
    _                 -> do
      putStrLn "<!>"
      pure (accX, accY)

  compareAlgorithms fs acc



