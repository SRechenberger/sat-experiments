module Formula3CNF where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (intercalate)

-- import Control.Arrow ((>>>), (<<<))

import Assignment


class Satisfies a where
  satisfies :: Assignment -> a -> Bool


data Literal
  = Pos Int
  | Neg Int
  deriving (Eq, Ord)

getLit :: Literal -> Int
getLit (Pos x) = x
getLit (Neg x) = x

instance Show Literal where
  show (Pos i) = "l_" ++ show i
  show (Neg i) = "¬l_" ++ show i


newtype Clause = Clause (Literal, Literal, Literal)
  deriving (Eq, Ord)


instance Show Clause where
  show (Clause (c1,c2,c3)) = "(" ++ show c1 ++ " , " ++ show c2 ++ " , " ++ show c3 ++ ")"


instance Satisfies Clause where
  satisfies a (Clause (l1,l2,l3)) = testLiteral l1 || testLiteral l2 || testLiteral l3
    where
      testLiteral :: Literal -> Bool
      testLiteral (Pos x) = testVar x a
      testLiteral (Neg x) = not $ testVar x a


data Formula = Formula Int (Set Clause)


instance Show Formula where
  show (Formula _ cls) = intercalate " , " (map show (Set.toList cls))


instance Satisfies Formula where
  satisfies a (Formula _ cls) = all (satisfies a) cls



