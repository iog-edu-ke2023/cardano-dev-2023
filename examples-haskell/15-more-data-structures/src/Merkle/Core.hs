module Merkle.Core
    ( NonEmpty (..)
    , Binary
    , MerkleTree (..)
    , height
    , treeHash
    , leaf
    , node
    , partial
    , construct
    ) where

import           Data.Binary          (Binary, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty   (NonEmpty (..))

import           Merkle.Hash          (Hash, hash, hashToByteString)

data MerkleTree a =
      Leaf Hash a
    | Node Int Hash (MerkleTree a) (MerkleTree a)
    | Partial Int Hash (MerkleTree a)
    deriving Show

height :: MerkleTree a -> Int
height (Leaf _ _)      = 0
height (Node n _ _ _)  = n
height (Partial n _ _) = n

treeHash :: MerkleTree a -> Hash
treeHash (Leaf h _)      = h
treeHash (Node _ h _ _ ) = h
treeHash (Partial _ h _) = h

leaf :: Binary a => a -> MerkleTree a
leaf a = Leaf (hash $ encode a) a

node :: MerkleTree a -> MerkleTree a -> MerkleTree a
node l r =
    let n = 1 + max (height l) (height r)
        h = hash $ hashToByteString (treeHash l) `LBS.append` hashToByteString (treeHash r)
    in  Node n h l r

partial :: MerkleTree a -> MerkleTree a
partial l =
    let n = 1 + height l
        bs = hashToByteString $ treeHash l
        h = hash $ bs `LBS.append` bs
    in  Partial n h l

construct :: Binary a => NonEmpty a -> MerkleTree a
construct = go . fmap leaf
  where
    go :: NonEmpty (MerkleTree a) -> MerkleTree a
    go (t :| [])     = t
    go (l :| r : ts) = go $ node l r :| step ts

    step :: [MerkleTree a] -> [MerkleTree a]
    step []           = []
    step [l]          = [partial l]
    step (l : r : ts) = node l r : step ts
