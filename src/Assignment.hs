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
{-# INLINE varCnt #-}


randomAssignment :: StdGen -> Int -> (Assignment, StdGen)
randomAssignment gen vars = (assignment,gen')
  where
    (assg,gen') = randomR (0,2^vars'-1) gen

    vars' :: Integer
    vars' = toEnum vars

    assignment :: Assignment
    assignment = Assignment vars assg


flipVar :: Int -> Assignment -> Assignment
flipVar idx (Assignment vars assg) = Assignment vars (assg `complementBit` idx)
{-# INLINE flipVar #-}

testVar :: Int -> Assignment -> Bool
testVar idx (Assignment vars assg) = testBit assg idx
{-# INLINE testVar #-}

streamOfRandomAssignments :: StdGen -> Int -> [Assignment]
streamOfRandomAssignments gen len = assg : streamOfRandomAssignments gen' len
  where
    (assg, gen') = randomAssignment gen len
