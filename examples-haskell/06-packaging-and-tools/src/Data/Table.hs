module Data.Table
    ( Table
    , empty
    , insert
    , delete
    , lookup
    , map
    ) where

import           Prelude hiding (lookup)

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
