module MerkleTree where

import qualified Crypto.Hash          as Crypto
import           Data.Binary          (Binary, encode)
import qualified Data.ByteArray       as BA
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

type Hash = Crypto.Digest Crypto.SHA256

hashToByteString :: Hash -> ByteString
hashToByteString = LBS.pack . BA.unpack

hash :: Binary a => a -> Hash
hash = Crypto.hashlazy . encode

data MerkleTree a = Leaf Hash a | Node Int Hash (MerkleTree a) (MerkleTree a)
    deriving Show

treeHash :: MerkleTree a -> Hash
treeHash (Leaf h _)     = h
treeHash (Node _ h _ _) = h

treeCount :: MerkleTree a -> Int
treeCount (Leaf _ _)     = 1
treeCount (Node n _ _ _) = n

leaf :: Binary a => a -> MerkleTree a
leaf a = Leaf (hash a) a

node :: MerkleTree a -> MerkleTree a -> MerkleTree a
node l r =
    Node
        (treeCount l + treeCount r)
        (hash $ hashToByteString (treeHash l) <> hashToByteString(treeHash r))
        l
        r

construct :: Binary a => [a] -> MerkleTree a
construct = go . map leaf
  where
    go :: [MerkleTree a] -> MerkleTree a
    go []  = error "no data"
    go [t] = t
    go xs  = go $ step xs

    step :: [MerkleTree a] -> [MerkleTree a]
    step []           = []
    step [_]          = error "number of payloads is not a power of 2"
    step (x : y : xs) = node x y : step xs

--             N

--      N           N

--   N     N     N     N
-- L1 L2 L3 L4 L5 L6 L7 L8
-- 0  1  2  3  4  5  6  7

data MerkleProof a = MerkleProof [Either Hash Hash] a
    deriving Show

createMerkleProof :: MerkleTree a -> Int -> MerkleProof a
createMerkleProof (Leaf _ a) i
    | i == 0    = MerkleProof [] a
    | otherwise = error "index out of bounds"
createMerkleProof (Node n _ l r) i
    | i < m     =
          let
            MerkleProof xs a = createMerkleProof l i
          in
            MerkleProof (Left (treeHash r) : xs) a
    | otherwise =
          let
            MerkleProof xs a = createMerkleProof r $ i - m
          in
            MerkleProof (Right (treeHash l) : xs) a
  where
    m = div n 2

exTree :: MerkleTree String
exTree = construct ["Haskell", "Scala", "Java", "Python", "C", "C++", "Forth", "OCaml"]

exRoot :: Hash
exRoot = treeHash exTree

exProof :: MerkleProof String
exProof = createMerkleProof exTree 4

--                        N
--              N                  N
--        N          N        N         N
-- Haskell Scala Java Python C C++ Forth OCaml
--    0      1     2    3    4  5    6     7

checkMerkleProof :: Binary a => Hash -> MerkleProof a -> Bool
checkMerkleProof merkleRoot proof = checkMerkleProof' proof == merkleRoot

checkMerkleProof' :: Binary a => MerkleProof a -> Hash
checkMerkleProof' (MerkleProof [] a)            = hash a
checkMerkleProof' (MerkleProof (Left h : xs) a) =
  let
    h' = checkMerkleProof' (MerkleProof xs a)
  in
    hash $ hashToByteString h' <> hashToByteString h
checkMerkleProof' (MerkleProof (Right h : xs) a) =
  let
    h' = checkMerkleProof' (MerkleProof xs a)
  in
    hash $ hashToByteString h <> hashToByteString h'



-- apple
-- apples
-- apply
-- enter
-- emerald
-- empty

--           *
--       a         e
--       p       m    n
--       p     e   p  t
--       l     r   t  e
--     e   y   a   y  r
--     s       l
--             d


-- Semigroup law: associativity
-- a <> (b <> c) = (a <> b) <> c

-- Monoid law: identity/neutral element:
-- a <> mempty = mempty <> a = a
