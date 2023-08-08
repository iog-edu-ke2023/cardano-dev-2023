module DatatypesAndFunctions where

import           Prelude hiding (elem, filter, length, lookup, not, reverse,
                          (++), (||))

not :: Bool -> Bool
not True  = False
not False = True

(||) :: Bool -> Bool -> Bool
(||) True  _ = True
(||) False y = y

(|||) :: Bool -> Bool -> Bool
(|||) True  True  = True
(|||) True  False = True
(|||) False True  = True
(|||) False False = False

loop :: a
loop = loop

-- undefined :: a

-- error :: String -> a

ifthenelse :: Bool -> a -> a -> a
ifthenelse c t e
    | c         = t
    | otherwise = e

myDiv :: Double -> Double -> Maybe Double
myDiv x y
    | y == 0    = Nothing
    | otherwise = Just (x / y)

fromMaybe :: a -> Maybe a -> a
fromMaybe _   (Just x) = x
fromMaybe def Nothing  = def

orelse :: Maybe a -> Maybe a -> Maybe a
orelse (Just x) _ = Just x
orelse Nothing  y = y

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just a) = Just (f a)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe _        _        = Nothing

liftMaybe :: (a -> b -> c)
          -> Maybe a
          -> Maybe b -- my third argument
          -> Maybe c
{-
liftMaybe f (Just a) (Just b) = Just (f a b)
liftMaybe _ _        _        = Nothing
-}

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

foo :: (a, b, c, d) -> (d, c, b, a)
foo (a, b, c, d) = (d, c, b, a)

foo' :: a -> b -> c -> d -> (d, c, b, a)
foo' a b c d = (d, c, b, a)

liftMaybe f x y = f <$> x <*> y

length :: [a] -> Int
length []       = 0
length (_ : xs) = 1 + length xs

elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem x (y : ys) = (x == y) || elem x ys

(++) :: [a] -> [a] -> [a]
(++) []       ys = ys
(++) (x : xs) ys =  x : (xs ++ ys)

reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ [x]

-- [1, 2, 3, 4]
-- (1 : [2, 3, 4])

-- reverse [2, 3, 4] = [4, 3, 2]
-- want: [4, 3, 2, 1] = [4, 3, 2] ++ [1]

-- reverse [1, 2, 3]
-- = reverse [2, 3]               ++ [1]
-- = (reverse [3]         ++ [2]) ++ [1]
-- = ((reverse [] ++ [3]) ++ [2]) ++ [1]
-- = (([]         ++ [3]) ++ [2]) ++ [1]
-- = [3, 2, 1]

filter :: (a -> Bool) -> [a] -> [a]
filter _ []       = []
filter p (x : xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs


newtype Table k v = Table [(k, v)]
    deriving (Show)

unTable :: Table k v -> [(k, v)]
unTable (Table xs) = xs

empty :: Table k v
empty = Table []

insert :: k -> v -> Table k v -> Table k v
insert k v (Table xs) = Table ((k, v) : xs)

delete :: Eq k => k -> Table k v -> Table k v
delete _ (Table []) = Table []
delete k (Table ((k', v) : kvs))
    | k' == k   = delete k (Table kvs)
    | otherwise = Table ((k', v) : unTable (delete k (Table kvs)))

lookup :: Eq k => k -> Table k v -> Maybe v
lookup _ (Table [])     = Nothing
lookup k (Table ((k', v) : kvs))
    | k' == k   = Just v
    | otherwise = lookup k (Table kvs)

languages :: Table String Bool
languages = insert "Haskell" True (insert "Java" False (insert "Haskell" True empty))

data Transaction = Transaction
    { trAmount :: Amount
    , trFrom   :: Account
    , trTo     :: Account
    } deriving (Show, Eq)

type Amount = Int
type Account = String

tx, tx' :: Transaction
--tx = Transaction 100 "Lars" "Karina"
tx = Transaction
    { trAmount = 100
    , trTo     = "Karina"
    , trFrom   = "Lars"
    }
tx' = tx {trAmount = 200}

type Accounts = Table Account Amount

processTransaction :: Transaction -> Accounts -> Accounts
processTransaction (Transaction amt f t) accounts
    | amt < 0   = accounts
    | otherwise =
        let
            fOld = fromMaybe 0 (lookup f accounts)
            tOld = fromMaybe 0 (lookup t accounts)
        in
            if fOld >= amt
                then insert f (fOld - amt) (insert t (tOld + amt) accounts)
                else accounts

accs :: Accounts
accs = insert "Karina" 1000 (insert "Lars" 250 empty)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

tree :: Tree Int
tree = Node (Leaf 7) (Node (Leaf 4) (Leaf 9))

flatten :: Tree a -> [a]
flatten (Leaf a)   = [a]
flatten (Node l r) = flatten l ++ flatten r

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

height :: Tree a -> Int
height (Leaf _)   = 0
height (Node l r) = 1 + max (height l) (height r)

data Expr a =
      Lit a
    | Add (Expr a) (Expr a)
    | Neg (Expr a)
    | IfZero (Expr a) (Expr a) (Expr a)
    deriving Show

expr :: Expr Double
expr = IfZero (Lit 7) (Lit 999) (Add (Neg (Lit 2)) (Lit 5))

eval :: (Eq a, Num a) => Expr a -> a
eval (Lit n)        = n
eval (Add x y)      = eval x + eval y
eval (Neg x)        = - (eval x)
eval (IfZero x y z)
    | eval x == 0   = eval y
    | otherwise     = eval z
