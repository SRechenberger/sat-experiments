module Formula3CNF where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (intercalate)

-- import Control.Arrow ((>>>), (<<<))

import Assignment


class Satisfies a where
  satisfies :: Assignment -> a -> Bool


data Literal
  = Pos !Int
  | Neg !Int
  deriving (Eq, Ord)

getLit :: Literal -> Int
getLit (Pos x) = x
getLit (Neg x) = x

instance Satisfies Literal where
  satisfies a (Pos x) = testVar x a
  satisfies a (Neg x) = not $ testVar x a

instance Show Literal where
  show (Pos i) = "l_" ++ show i
  show (Neg i) = "¬l_" ++ show i


newtype Clause = Clause (Literal, Literal, Literal)
  deriving (Eq, Ord)


instance Show Clause where
  show (Clause (c1,c2,c3)) = "(" ++ show c1 ++ " , " ++ show c2 ++ " , " ++ show c3 ++ ")"


instance Satisfies Clause where
  satisfies a (Clause (l1,l2,l3)) = satisfies a l1 || satisfies a l2 || satisfies a l3

satisfiedLiterals :: Assignment -> Clause -> Int
satisfiedLiterals a (Clause (l1,l2,l3)) = ind (satisfies a l1) + ind (satisfies a l2) + ind (satisfies a l3)
  where
    ind True  = 1
    ind False = 0

data Formula = Formula Int !(Set Clause)


instance Show Formula where
  show (Formula _ cls) = intercalate " , " (map show (Set.toList cls))


instance Satisfies Formula where
  satisfies a (Formula _ cls) = all (satisfies a) cls



