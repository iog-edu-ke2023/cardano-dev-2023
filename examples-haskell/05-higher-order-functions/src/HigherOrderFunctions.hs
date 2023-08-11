{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE InstanceSigs   #-}

module HigherOrderFunctions
    ( foo
    , Chain (..)
    ) where

import qualified Data.Char     as C
import           Data.Foldable (Foldable (foldr))
import           Data.List     (foldl')
import           Prelude       hiding (all, any, filter, foldl, foldr, reverse,
                                (++), (.))
foo :: Char -> String
foo c = [C.toUpper c, c]


data Chain txs =
      GenesisBlock
    | Block (Chain txs) txs
    deriving (Show, Eq, Functor, Foldable)

{-
instance Eq txs => Eq (Chain txs) where

    (==) :: Chain txs -> Chain txs -> Bool
    GenesisBlock == GenesisBlock = True
    Block xs x   == Block ys y   = xs == ys && x == y
    _            == _            = False
-}

isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
isPrefixOf GenesisBlock GenesisBlock   = True
isPrefixOf (Block _ _)  GenesisBlock   = False
isPrefixOf c            d@(Block xs _) =
    c `isPrefixOf` xs || c == d

allEquals :: Eq a => [a] -> Bool
allEquals []               = True
allEquals [_]              = True
allEquals (x : xs@(y : _)) = x == y && allEquals xs

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

example1 :: [Integer]
example1 =
    (take 100 . filter odd . map (\x -> x * x)) [1 ..]

foreach :: [a] -> (a -> b) -> [b]
foreach = flip map

example2 :: [Int]
example2 = foreach [1 .. 10] (2 *)
--       = map (2 *) [1 .. 10]

-- f :: a -> b
-- \a -> f a
-- those are the same! ("eta reduction")

foldr' :: (a -> r -> r) -> r -> [a] -> r
foldr' cons nil = go
  where
    --go :: [a] -> r
    go []       = nil
    go (y : ys) = cons y $ go ys

length' :: [a] -> Int
length' = foldr (const (+ 1)) 0

filter :: (a -> Bool) -> [a] -> [a]
filter p =
    foldr (\x ys -> if p x then x : ys else ys) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

all, any :: [Bool] -> Bool
all = foldr' (&&) True
any = foldr' (||) False

idList :: [a] -> [a]
idList = foldr (:) []

reverse' :: [a] -> [a]
reverse' = foldr (\x ys -> ys ++ [x]) []

-- reverse' [1, 2, 3]
-- reverse' (1 : 2 : 3 : [])
-- reverse' (2 : 3 : []) ++ (1 : [])
-- (reverse' (3 : []) ++ (2 : [])) ++ (1 : [])
-- (reverse' [] ++ (3 : [])) ++ (2 : [])) ++ (1 : [])
-- ([] ++ (3 : [])) ++ (2 : [])) ++ (1 : [])
-- ((3 : [])) ++ (2 : [])) ++ (1 : [])
-- (3 : ([] ++ (2 : []))) ++ (1 : [])
-- (3 : (2 : [])) ++ (1 : [])
-- 3 : ((2 : []) ++ (1 : []))
-- 3 : (2 : ([] ++ (1 : [])))
-- 3 : (2 : (1 : []))
-- [3, 2, 1]

reverse :: [a] -> [a]
reverse = go []
  where
    go :: [a] -> [a] -> [a]
    go acc []       = acc
    go acc (x : xs) = go (x : acc) xs

-- reverse [1, 2, 3]
-- go [] [1, 2, 3]
-- go (1 : []) [2, 3]
-- go (2 : 1 : []) [3]
-- go (3 : 2 : 1 : []) []
-- 3 : 2 : 1 : []
-- [3, 2, 1]

sum' :: Num a => [a] -> a
sum' = go 0
  where
    go acc []       = acc
    go acc (x : xs) = go (x + acc) xs

foldl :: (r -> a -> r) -> r -> [a] -> r
foldl op = go
  where
    go acc []       = acc
    go acc (x : xs) = go (op acc x) xs

foldl'' :: (r -> a -> r) -> r -> [a] -> r
foldl'' op = go
  where
    go acc []       = acc
    go acc (x : xs) =
      let
        !acc' = op acc x
      in
        go acc' xs


sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0

turboSum :: Num a => [a] -> a
turboSum = foldl' (+) 0

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

{-
instance Foldable Chain where
    foldr :: (a -> r -> r) -> r -> Chain a -> r
    foldr _  e GenesisBlock = e
    foldr op e (Block c a)  = op a $ foldr op e c
-}

chain1 :: Chain Int
chain1 = Block (Block (Block GenesisBlock 3) 4) 10

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Functor, Foldable)

{-
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a)   = Leaf $ f a
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

instance Functor Tree where
    fmap = mapTree
-}

tree :: Tree Int
tree = Node (Leaf 7) (Node (Leaf 4) (Leaf 9))

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f = maybe Nothing $ Just . f
{-
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just a) = Just $ f a
-}

(|||) :: Bool -> Bool -> Bool
x ||| y = if x then True else y
{-
(|||) True  _ = True
(|||) False y = y
-}

treeCata :: (a -> r) -> (r -> r -> r) -> Tree a -> r
treeCata leaf node = go
  where
    go (Leaf a)   = leaf a
    go (Node l r) = node (go l) (go r)

flatten :: Tree a -> [a]
flatten = treeCata (\a -> [a]) (++)
