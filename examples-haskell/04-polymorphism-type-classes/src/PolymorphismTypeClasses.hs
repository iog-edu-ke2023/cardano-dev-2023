{-# LANGUAGE InstanceSigs #-}
module PolymorphismTypeClasses where

import           Prelude hiding (fst, sum)

fst :: (a, b) -> a
fst (x, _) = x

restrictedFst :: (Int, Int) -> Int
restrictedFst = fst

-- fst' :: (a, b) -> a
-- fst' = restrictedFst

fst' :: (a, a) -> a
fst' = fst

foo1, foo2, foo3, foo4, foo5 :: (Int, Int) -> (Int, Int)
foo1 (x, y) = (x + 1, y + 1)
foo2 (x, y) = (y, x)
foo3 (x, y) = (2 * x, 2 * y)
foo4 (x, y) = (x + y + 42, y - 2 * x)
foo5 (_, _) = (1, 2)
-- ...

bar1, bar2, bar3, bar4 :: (a, a) -> (a, a)
bar1 (x, y) = (y, x)
bar2 (x, y) = (x, y)
bar3 (x, _) = (x, x)
bar4 (_, y) = (y, y)

baz :: (a, b) -> (b, a)
baz (x, y) = (y, x)



parse :: String -> Either Bool Int
parse "False" = Left False
parse "0"     = Right 0
parse s       = error $ "can't parse " ++ s

{-
class Eq a where
    (==) :: a -> a -> Bool
    x == y = not (x /= y)

    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    {-# MINIMAL (==) | (/=) #-}


instance Eq Bool where

    True  == True  = True
    False == False = True
    _     == _     = False

instance Eq a => Eq [a] where
    []       == []       = True
    (x : xs) == (y : ys) = x == y && xs == ys
    _        == _        = False
    -}

allEqual :: Eq a => [a] -> Bool
allEqual []           = True
allEqual [_]          = True
allEqual (x : y : zs) = x == y && allEqual (y : zs)

allEqual' :: (a -> a -> Bool) -> [a] -> Bool
allEqual' _  []           = True
allEqual' _  [_]          = True
allEqual' eq (x : y : zs) = eq x y && allEqual' eq (y : zs)

foo :: Show a => a -> String
foo a = "Kenya says hi to " ++ show a

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Read, Eq, Ord)

strange :: String -> String
strange x = show (read x :: Bool)

-- elem :: Eq a => a -> ([a] -> Bool)

add3 :: Int -> (Int -> (Int -> Int))
add3 = \x -> (\y -> (\z -> x + y + z))

add3' :: (Int, Int, Int) -> Int
add3' (x, y, z) = x + y + z

-- add3 5   = (\x -> (\y -> (\z -> x + y + z))) 5
--          = \y -> (\z -> 5 + y + z)
-- add3 5 7 = (\y -> (\z -> 5 + y + z)) 7
--          = \z -> 5 + 7 + z
--          = \z -> 12 + z

sum :: [Int] -> Int
sum []       = 0
sum (x : xs) = x + sum xs

-- sum [1,2,3] = sum (1 : 2 : 3 : []) = 1 + sum (2 : 3 : [])
--   = 1 + (2 + sum (3 : []))
--   = 1 + (2 + (3 + sum []))
--   = 1 + (2 + (3 + 0))
--   = 6

flatten :: Tree a -> [a]
flatten (Leaf a)   = [a]
flatten (Node l r) = flatten l ++ flatten r

example :: [Int]
example = flatten (Node (Node (Leaf 1) (Leaf 2)) (Leaf 7))

-- example = flatten (Node (Leaf 1) (Leaf 2)) ++ flatten (Leaf 7)
--   = (flatten (Leaf 1) ++ flatten (Leaf 2)) ++ [7]
--   = ([1] ++ [2]) ++ [7]
--   = [1, 2, 7]
