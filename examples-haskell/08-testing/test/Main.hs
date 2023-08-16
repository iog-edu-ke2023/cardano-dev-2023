module Main
    ( main
    ) where

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Testing

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Haskell" $ do
        it "does correct arithmetic" $
            3 + 5 == (8 :: Int)
    describe "sort" $ do
        prop "preserves lengths" $
            sort `preserves` length
        prop "ensures isSorted" $
            sort `ensures` isSorted
