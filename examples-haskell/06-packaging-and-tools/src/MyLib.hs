module MyLib where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Table (Table, map)
import qualified Data.Table as T

table :: Table String Bool
table = T.insert "Haskell" True  $
        T.insert "Java"    False $
        T.insert "Python"  False $
        T.insert "Plutus"  True  T.empty

table' :: Map String Bool
table' = M.insert "Haskell" True  $
         M.insert "Java"    False $
         M.insert "Python"  False $
         M.insert "Plutus"  True  M.empty

foo :: [Int]
foo = map (+ 1) [1 .. 10]
