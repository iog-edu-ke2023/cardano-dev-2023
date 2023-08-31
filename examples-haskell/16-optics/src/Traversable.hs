{-# LANGUAGE InstanceSigs #-}

module Traversable where

import           Control.Monad       (ap, liftM)
import           Control.Monad.State (State, evalState, get, put)
import           Data.Foldable       (Foldable (foldMap))
import           Prelude             hiding (Traversable (..), foldMap, map,
                                      mapM)

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap = liftM

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) = ap

instance Monad Identity where
    return :: a -> Identity a
    return = pure

    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    Identity a >>= k = k a

fifty :: Int
fifty = runIdentity $ do
    x <- Identity 42
    let y = 8
    pure $ x + y

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ []       = pure []
mapM f (x : xs) = do
    b  <- f x
    bs <- mapM f xs
    pure $ b : bs

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA _ []       = pure []
mapA f (x : xs) = (:) <$> f x <*> mapA f xs

map :: (a -> b) -> [a] -> [b]
map f = runIdentity . mapA (Identity . f)

-- foldMap f [x1, x2, ...]
-- f x1 <> f x2 <> ...

foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap f = getConst . mapA (Const . f)

newtype Const m b = Const {getConst :: m}

instance Functor (Const m) where
    fmap :: (a -> b) -> Const m a -> Const m b
    fmap _ (Const m) = Const m

instance Monoid m => Applicative (Const m) where

    pure :: a -> Const m a
    pure _ = Const mempty

    (<*>) :: Const m (a -> b) -> Const m a -> Const m b
    Const f <*> Const a = Const $ f <> a

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

tree :: Tree String
tree = Node
    (Leaf "Haskell")
    (Node
        (Leaf "is")
        (Leaf "beautiful"))

mapT :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
mapT f (Leaf a)   = Leaf <$> f a
mapT f (Node l r) = Node <$> mapT f l <*> mapT f r

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f = runIdentity . mapT (Identity . f)

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f = getConst . mapT (Const . f)

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable [] where
    traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
    traverse = mapA

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse = mapT

fmapDefault :: Traversable t => (a -> b) -> t a -> t b
fmapDefault f = runIdentity . traverse (Identity . f)

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . traverse (Const . f)

labelTree :: Tree a -> Tree (Int, a)
labelTree t = evalState (traverse f t) 1
  where
    f :: a -> State Int (Int, a)
    f a = do
        n <- get
        put $ n + 1
        pure (n, a)

-- traverse (map toUpper) tree
-- map toUpper :: String -> String = [Char]
--                   a                f b
-- f = []
-- b = Char

-- traverse (map toUpper) tree :: f (Tree b) = [Tree Char]
