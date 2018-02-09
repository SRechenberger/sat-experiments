module Main (main) where

import Assignment
import Formula3CNF
import Generator
import Solver

import System.Random
import System.Environment (getArgs)

import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.List (partition, minimumBy, maximumBy)

import Control.Monad (forM)
import Control.Arrow
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar)

import Text.Printf

-- import Data.Maybe (isJust)
-- import Data.List (intercalate)


main :: IO ()
main = do
  -- Setup parameters
  args@[e', n', gamma', cb', eps', s] <- getArgs
  print args
  let n     = read n'                       -- number of variables
      e     = read e'                       -- number of experiments
      gamma = read gamma' :: Double         -- relation m/n
      m     = fromEnum $ toEnum n * gamma   -- number of clauses
      l     = n                             -- maximum of flips per try
      fl    = fromEnum $ 20*(4/3)^n         -- total maximum of flips
      t     = fl `div` l                    -- maximum of tries

      cb    = read cb'                      -- break weight
      eps   = read eps'
  let scr = case s of
              "poly" -> scorePoly cb eps
              "exp" -> scoreExp cb eps
              other -> error $ "Option \"" ++ other ++"\" is no valid option."

  -- Generating Formulas
  genF <- getStdGen
  let fs = take e (streamOfRandom3CNF genF n m)

  results' <- forM (fs `zip` [1..]) $ \(f,i) -> do 
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    -- generate supply of random assignments
    (genA,genS) <- split <$> getStdGen
    let as = streamOfRandomAssignments genA n

    -- Sparking thread for normal probSAT
    forkIO $ do
      let r = fst $ probSAT scr genS t l as f 
      putMVar mvar1 r
      case r of
        Just (a,fl) -> printf "n Thread %-4d: %5d flips.\n" (2*i :: Int) fl
        Nothing     -> printf "n Thread %-4d found nothing.\n" (2*i :: Int)
      -- putStrLn $ "probSAT finished formula " ++ show i 

    -- Sparking thread for probSAT with entropy heuristic
    forkIO $ do
      let r = fst $ probSATWithEntropy scr genS t l as f 
      putMVar mvar2 r
      case r of
        Just (a,fl) -> printf "h Thread %-4d: %5d flips.\n" (2*i+1 :: Int) fl
        Nothing     -> printf "h Thread %d found nothing.\n" (2*i+1 :: Int)
      -- putStrLn $ "probSATWithEntropy finished formula " ++ show i 

    -- putStrLn $ "Threads for Formula " ++ show i ++ " sparked."
    pure (f, mvar1, mvar2)
  
  -- Receiving results
  results <- mapM (\(f, mv1, mv2) -> (,,) f <$> readMVar mv1 <*> readMVar mv2) results'

  -- evaluate results
  finalResult' <- forM results $ \(f, r1, r2) -> do
    case (r1, r2) of
      (Just (a1, fl1), Just (a2, fl2))
        -- For debuggin reasons (who knows, what may happen...)
        | not (a1 `satisfies` f) -> do
            error $ "probSAT error: " ++ show a1 ++ " does not satisfy " ++ show f
        | not (a2 `satisfies` f) -> do
            error $ "probSATWithEntropy error: " ++ show a2 ++ " does not satisfy " ++ show f
        -- expected cases
        | otherwise -> pure (fl1, fl2)

      (Just (a1, fl1), Nothing)
        | not (a1 `satisfies` f) -> do
            error $ "probSAT error: " ++ show a1 ++ " does not satisfy " ++ show f
        | otherwise -> pure (fl1, t*l*10)

      (Nothing, Just (a2, fl2))
        | not (a2 `satisfies` f) -> do
            error $ "probSATWithEntropy error: " ++ show a2 ++ " does not satisfy " ++ show f
        | otherwise -> pure (t*l*10, fl2)

      (Nothing, Nothing) -> do
            pure (t*l*10, t*l*10)
  
  -- evaluate final results
  let r = unzip finalResult'
      (avPSat, avPSatH) = map toEnum *** map toEnum >>> avg *** avg $ r
      (minPSat, minPSatH) = minimum *** minimum $ r
      (maxPSat, maxPSatH) = maximum *** maximum $ r

  printf "normal probSAT:  average = %7.2f minimum = %5d maximum = %5d\n" avPSat minPSat maxPSat
  printf "entropy-probSAT: average = %7.2f minimum = %5d maximum = %5d\n" avPSatH minPSatH maxPSatH


avg :: [Double] -> Double
avg ds = sum ds / toEnum (length ds)
