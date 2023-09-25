{-# LANGUAGE TemplateHaskell #-}

module UsePower where

import           Control.Lens.TH (makeLenses)
import           Numeric.Natural (Natural)
import           Power

exp8 :: Int
exp8 = $$exp2

{-
naiverPower10 :: Integer -> Integer
naiverPower10 x = $$(naivePower' 10 [|| x ||])
-}

spower10 :: Integer -> Integer
spower10 x = $$(spower 10 [|| x ||])

{-
spower100 :: Integer -> Integer
spower100 x = $$(spower 100 [|| x ||])
-}

spower10' :: Integer -> Integer
spower10' = $$(to $ spower 10)

memoFib :: Natural -> Natural
memoFib = $$(smemo [0 .. 5] fib [|| fib ||])

properFib :: Natural -> Natural
properFib n = fibs !! fromIntegral n

fibs :: [Natural]
fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)

-- useTE = $te

$(myFsts [4 .. 20])

fibsInfo :: String
fibsInfo = $(showNameInfo 'fibs)

treeInfo :: String
treeInfo = $(showNameInfo ''Tree)

data Person = Person
    { _name    :: String
    , _age     :: Int
    , _address :: Address
    }


data Address = Address
    { _country :: String
    , _city    :: String
    }

makeLenses ''Person
makeLenses ''Address























{-
power10 :: Int -> Int
power10 = $$(spower' 10)
-}
