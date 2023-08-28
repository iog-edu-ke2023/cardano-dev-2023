module Merkle
    ( module X
    , exampleTree
    , rustPath
    ) where

import           Merkle.Core as X
import           Merkle.Hash as X
import           Merkle.Path as X

exampleTree :: MerkleTree String
exampleTree = construct $ "Haskell" :| ["Rust", "Python", "Java", "C", "Lisp"]

rustPath :: MerklePath String
rustPath = case merklePath exampleTree 1 of
    Nothing -> error "this shouldn't happen"
    Just p  -> p
