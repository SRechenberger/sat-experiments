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
  args@[n', e', gamma', cb', cm', eps', s] <- getArgs
  print args
  let n     = read n'                       -- number of variables
      e     = read e'                       -- number of experiments
      gamma = read gamma' :: Double         -- relation m/n
      m     = fromEnum $ toEnum n * gamma   -- number of clauses
      t     = 20 * fromEnum ((4/3)^n)       -- number of tries
      l     = 3*n                           -- number of flips

      cb    = read cb'                      -- break weight
      cm    = read cm'                      -- make weight
      eps   = read eps'
  let scr = case s of
              "poly" -> scorePoly cm cb eps
              "exp" -> scoreExp cm cb eps
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
      putStrLn $ "probSAT finished formula " ++ show i 
      putMVar mvar1 r

    -- Sparking thread for probSAT with entropy heuristic
    forkIO $ do
      let r = fst $ probSATWithEntropy scr genS t l as f 
      putStrLn $ "probSATWithEntropy finished formula " ++ show i 
      putMVar mvar2 r

    putStrLn $ "Threads for Formula " ++ show i ++ " sparked."
    pure (f, mvar1, mvar2)
  
  -- Receiving results
  results <- mapM (\(f, mv1, mv2) -> (,,) f <$> readMVar mv1 <*> readMVar mv2) results'

  -- evaluate results
  finalResult' <- forM results $ \(f, r1, r2) -> do
    case (r1, r2) of
      (Just (a1, fl1), Just (a2, fl2))
        -- For debuggin reasons (who knows, what may happen...)
        | not (a1 `satisfies` f) -> do
            putStrLn $ "!! probSAT error: " ++ show a1 ++ " does not satisfy " ++ show f
            pure Nothing
        | not (a2 `satisfies` f) -> do
            putStrLn $ "!! probSATWithEntropy error: " ++ show a2 ++ " does not satisfy " ++ show f
            pure Nothing
        -- expected cases
        | fl1 <= fl2 -> do
            let p = 1 - percents fl1 fl2
            putStrLn $ "probSAT was\t" ++ (printf "%.2f" (p*100)) ++ "% faster (" ++ show fl1 ++ " flips vs " ++ show fl2 ++ " flips)" 
            pure (Just (p, fl1, fl2, False))
        | fl1 > fl2 -> do
            let p = 1 - percents fl2 fl1
            putStrLn $ "probSATWithEntropy was\t" ++ (printf "%.2f" (p*100)) ++ "% faster (" ++ show fl2 ++ " flips vs " ++ show fl1 ++ " flips)"
            pure (Just (p, fl2, fl1, True))

      (Just (a1, fl1), Nothing)
        | not (a1 `satisfies` f) -> do
            putStrLn $ "!! probSAT error: " ++ show a1 ++ " does not satisfy " ++ show f
            pure Nothing
        | otherwise -> do
            putStrLn $ "probSATWithEntropy found no solution."
            let p = 1 - percents fl1 (t*l)
            pure (Just (p, fl1, t*l, False))

      (Nothing, Just (a2, fl2))
        | not (a2 `satisfies` f) -> do
            putStrLn $ "!! probSATWithEntropy error: " ++ show a2 ++ " does not satisfy " ++ show f
            pure Nothing
        | otherwise -> do
            putStrLn $ "probSAT found no solution."
            let p = 1 - percents fl2 (t*l)
            pure (Just (p, fl2, t*l, True))

      (Nothing, Nothing) -> do
            putStrLn $ "No solution found."
            pure (Just (0, t*l, t*l, False))
  
  -- evaluate final results
  let finalResult = catMaybes finalResult'
      (pSatH, pSat) = partition (\(_,_,_,b) -> b)
        >>> (let drp (a,b,c,_) = (a,b,c) in map drp *** map drp)
        $ finalResult
  putStrLn $ "Final Result of " ++ show e ++ " tests:"
  putStrLn $ "  Normal probSAT:" 
  putStrLn $ "    Tests won:\t" ++ show (length pSat)
  putStrLn $ let (rel,abs) = minAdvantage pSat
          in "    Minimum advantage:\t" ++ show abs ++ " (" ++ printf "%.2f" rel ++" %)" 
  putStrLn $ let (rel,abs) = maxAdvantage pSat
          in "    Maximum advantage:\t" ++ show abs ++ " (" ++ printf "%.2f" rel ++"%)"
  putStrLn $ let (rel,abs) = avgAdvantage pSat
          in "    Average advantage:\t" ++ show abs ++ " (" ++ printf "%.2f" rel ++"%)"
  putStrLn $ "  probSAT with entropy heuristic:" 
  putStrLn $ "    Tests won:\t" ++ show (length pSatH)
  putStrLn $ let (rel,abs) = minAdvantage pSatH
          in "    Minimum advantage:\t" ++ show abs ++ " (" ++ printf "%.2f" rel ++"%)" 
  putStrLn $ let (rel,abs) = maxAdvantage pSatH
          in "    Maximum advantage:\t" ++ show abs ++ " (" ++ printf "%.2f" rel ++"%)"
  putStrLn $ let (rel,abs) = avgAdvantage pSatH
          in "    Average advantage:\t" ++ show abs ++ " (" ++ printf "%.2f" rel ++"%)"




percents :: Int -> Int -> Double
percents a b = toEnum a / toEnum b 

minAdvantage :: [(Double, Int, Int)] -> (Double, Int)
minAdvantage = minimumBy (compare `on` fst3) >>> (\(p,f1,f2) -> (p,f2-f1))

maxAdvantage :: [(Double, Int, Int)] -> (Double, Int)
maxAdvantage = maximumBy (compare `on` fst3) >>> (\(p,f1,f2) -> (p,f2-f1))

avgAdvantage :: [(Double, Int, Int)] -> (Double, Double)
avgAdvantage results =
    map (\(p,f1,f2) -> (p,f2-f1))  -- [(Double, Int, Int)] -> [(Double, Int)]
    >>> unzip                      -- [(Double, Int)]      -> ([Double],[Int]) 
    >>> sum *** sum                -- ([Double],[Int])     -> (Double, Int)
    >>> second toEnum              -- (Double, Int)        -> (Double, Double)
    >>> (/ len) *** (/ len)        -- (Double, Double)     -> (Double, Double)
    $ results
  where
    len = toEnum $ length results

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c
