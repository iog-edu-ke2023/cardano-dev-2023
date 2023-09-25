{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Quasi
    ( ternary
    ) where

import           Data.List                 (foldl')
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

ternary :: QuasiQuoter
ternary = QuasiQuoter
    { quoteExp  = \s -> let n = parseTernary s in [| n |]
    , quotePat  = \s -> let n = parseTernary s in pure $ LitP $ IntegerL n
    , quoteType = error "can't use ternary literal as a type"
    , quoteDec  = error "can't use ternary literal as a declaration"
    }

parseTernary :: String -> Integer
parseTernary = foldl' f 0
  where
    f :: Integer -> Char -> Integer
    f acc '0' = 3 * acc
    f acc '1' = 3 * acc + 1
    f acc '2' = 3 * acc + 2
    f _   c   = error $ "invalid character: " ++ [c]


-- "1120" ---- 1 * 27 + 1 * 9 + 2 * 3 + 0 * 1
--        ---- 1 * 27 + 1 * 9 + 2 * 3 + 0
--        ---- 3 * (1 * 9 + 1 * 3 + 2 * 1) + 0
