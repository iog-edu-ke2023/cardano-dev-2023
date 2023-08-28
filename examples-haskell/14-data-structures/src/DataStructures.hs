{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module DataStructures where

import           Data.Kind (Type)
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Set  (Set)
import qualified Data.Set  as Set

type WrappedInt :: (Type -> Type) -> Type
newtype WrappedInt f = Wrap (f Int)

ex1 :: WrappedInt Maybe
ex1 = Wrap (Just 42)

ex2 :: WrappedInt []
ex2 = Wrap [1, 2, 5]

ex3 :: WrappedInt IO
ex3 = Wrap readLn

data MyWeirdPair a = MWP a a

swap :: forall a b. (a, b) -> (b, a)
swap (n, b) = (b, n)

fmap' :: (a -> b) -> (a, t) -> (b, t)
--fmap' f (a, t) = (f a, t)
fmap' f = swap . fmap f . swap

type Flip f a b = f b a

ex4 :: Flip (,) Int Bool
ex4 = (True, 42)

-- instance Functor (Flip (,) t) where
--    fmap = fmap'

ex5 :: [Char]
ex5 = 'x' : ex5

data List a = Nil | Cons a (List a)

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- A binary tree is call "perfect" if all its leaves have the same distance to the root.

-- Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)) is perfect.
--        .
--     .     .
--    1 2   3 4

-- Node (Leaf 1) (Node (Leaf 2) (Leaf 3)) is NOT perfect.

--         .
--       1   .
--          2  3

-- Can we define a datatype of perfect binary trees?


data Perfect a = Zero a | Suc (Perfect (a, a))
    deriving (Show)

tree1, tree2, tree3, tree4 :: Perfect Int
tree1 = Zero 1
tree2 = Suc (Zero (1, 2))
tree3 = Suc (Suc (Zero ((1, 2), (3, 4))))
tree4 = Suc (Suc (Suc (Zero (((1, 2), (3, 4)), ((5, 6), (7, 8))))))

sumPerfect :: Perfect Int -> Int
sumPerfect = sumPerfect' id

sumPerfect' :: forall a. (a -> Int) -> Perfect a -> Int
sumPerfect' f (Zero a) = f a
sumPerfect' f (Suc t)  = sumPerfect' g t
  where
    g :: (a, a) -> Int
    g (x, y) = f x + f y

mySum :: [Int] -> Int
mySum []       = 0
mySum (x : xs) = x + mySum xs

myTailRecursiveSum :: [Int] -> Int
myTailRecursiveSum = myTailRecursiveSum' id

myTailRecursiveSum' :: (Int -> Int) -> [Int] -> Int
myTailRecursiveSum' k []       = k 0
myTailRecursiveSum' k (x : xs) = myTailRecursiveSum' (\s -> k $ x + s) xs
