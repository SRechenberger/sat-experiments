module Assignment 
  ( Assignment
  , randomAssignment
  , flipVar
  , testVar
  , streamOfRandomAssignments
  , varCnt
  ) where

import Data.Bits (complementBit, testBit)
import System.Random (randomR, StdGen)


data Assignment = Assignment Int !Integer
  deriving (Show, Eq)

varCnt :: Assignment -> Int
varCnt (Assignment x _) = x

randomAssignment :: StdGen -> Int -> (Assignment, StdGen)
randomAssignment gen vars = (assignment,gen')
  where
    (assg,gen') = randomR (0,2^vars'-1) gen

    vars' :: Integer
    vars' = toEnum vars

    assignment :: Assignment
    assignment = Assignment vars assg


flipVar :: Int -> Assignment -> Assignment
flipVar idx (Assignment vars assg)
  | idx >= vars = error $ "flip: out of bounds: " ++ show idx ++ " >= " ++ show vars
  | idx < 0     = error $ "flip: negative index: " ++ show idx 
  | otherwise   = Assignment vars (assg `complementBit` idx)


testVar :: Int -> Assignment -> Bool
testVar idx (Assignment vars assg)
  | idx >= vars = error $ "testVar: out of bounds: " ++ show idx ++ " >= " ++ show vars
  | idx < 0     = error $ "testVar: negative index: " ++ show idx 
  | otherwise   = testBit assg idx


streamOfRandomAssignments :: StdGen -> Int -> [Assignment]
streamOfRandomAssignments gen len = assg : streamOfRandomAssignments gen' len
  where
    (assg, gen') = randomAssignment gen len
