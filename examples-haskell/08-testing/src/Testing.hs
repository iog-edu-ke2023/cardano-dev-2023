module Testing where

import           Control.Monad   (liftM2)
import           Data.List       (permutations)
import           Test.QuickCheck

sort :: [Int] -> [Int]
sort []       = []
sort (x : xs) = insert x $ sort xs

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : ys)
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys

sortPreservesLength, idPreservesLength :: [Int] -> Bool
sortPreservesLength = sort `preserves` length
idPreservesLength = id `preserves` length

preserves :: Eq a => (t -> t) -> (t -> a) -> t -> Bool
-- (f `preserves` p) xs = p (f xs) == p xs
preserves f p xs = p (f xs) == p xs

isSorted :: [Int] -> Bool
isSorted []           = True
isSorted [_]          = True
isSorted (x : y : zs) = x <= y && isSorted (y : zs)

ensures :: (t -> a) -> (a -> b) -> t -> b
(f `ensures` p) xs = p $ f xs

sortEnsuresSorted :: [Int] -> Bool
sortEnsuresSorted = sort `ensures` isSorted

evilNoSort :: [Int] -> [Int]
evilNoSort xs = replicate (length xs) 42

permutes :: Eq a => ([a] -> [a]) -> [a] -> Bool
f `permutes` xs = f xs `elem` permutations xs

appendLength :: [a] -> [a] -> Bool
appendLength xs ys = length (xs ++ ys) == length xs + length ys

plusIsCommutative :: Int -> Int -> Bool
plusIsCommutative x y = x + y == y + x

takeDrop :: Int -> [Int] -> Bool
takeDrop n xs = take n xs ++ drop n xs == xs

dropTwice :: Int -> Int -> [Int] -> Property
dropTwice m n xs = drop m (drop n xs) === drop (m + n) xs

lengthEmpty :: Bool
lengthEmpty = length [] == 0

implies :: Bool -> Bool -> Bool
implies x y = not x || y

insertPreservesOrdered :: Int -> [Int] -> Property
insertPreservesOrdered x xs =
    isSorted xs ==> isSorted (insert x xs)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)

-- A random tree of size n should have at most (n + 1) leaves.

-- generate a random tree with the given number of leaves.
genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree n
    | n <= 1    = Leaf <$> arbitrary
    | otherwise = do
        nl <- elements [1 .. n - 1]
        let nr = n - nl
        Node <$> genTree nl <*> genTree nr

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized $ \n -> do
        m <- elements [1 .. n + 1]
        genTree m

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> Gen a -> Gen b
-- fmap (Leaf :: a -> Tree a) = Gen a -> Gen (Tree a)
-- fmap Leaf arbitrary :: Gen (Tree a)

mkSorted :: [Int] -> [Int]
mkSorted []           = []
mkSorted [x]          = [x]
mkSorted (x : y : ys) = x : mkSorted (x + abs y : ys)

insertPreservesOrdered' :: Int -> [Int] -> Bool
insertPreservesOrdered' x xs =
    isSorted (insert x ys)
  where
    ys = mkSorted xs

genSorted :: Gen [Int]
genSorted = mkSorted <$> arbitrary

insertPreservesOrdered'' :: Int -> Property
insertPreservesOrdered'' x = forAll genSorted $ \xs ->
    isSorted (insert x xs)

newtype SortedIntList = SortedIntList [Int]
    deriving (Show)

instance Arbitrary SortedIntList where
    arbitrary = SortedIntList <$> genSorted

insertPreservesOrdered''' :: Int -> SortedIntList -> Bool
insertPreservesOrdered''' x (SortedIntList xs) =
    isSorted (insert x xs)

insertPreservesOrdered'''' :: Int -> OrderedList Int -> Bool
insertPreservesOrdered'''' x (Ordered xs) =
    isSorted (insert x xs)

dropTwiceNN :: NonNegative Int -> NonNegative Int -> [Int] -> Bool
dropTwiceNN (NonNegative m) (NonNegative n) xs
    = drop m (drop n xs) == drop (m + n) xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ []       = []
myMap f (x : xs) = f x : f x : myMap f xs


