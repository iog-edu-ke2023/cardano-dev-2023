{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE MagicHash      #-}

module MoreDataStructures where

import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           Data.Vector.Mutable  (IOVector)
import qualified Data.Vector.Mutable  as IOV
import           GHC.Prim
import           GHC.Types

unpackInt :: Int -> Int#
unpackInt (I# n) = n

ex :: Int
ex = I# (unpackInt $ 3)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Functor, Foldable)

exTree :: Tree String
exTree =
    Node
        (Node
            (Leaf "Haskell")
            (Leaf "Rust"))
        (Leaf "C")

instance Binary a => Binary (Tree a) where

    put :: Tree a -> Put
    put (Leaf a)   = putWord8 10 >> put a
    put (Node l r) = putWord8 11 >> put l >> put r

    get :: Get (Tree a)
    get = do
        w <- getWord8
        if w == 10
            then Leaf <$> get
            else if w == 11
                then Node <$> get <*> get
                else fail "unexpected tag"
