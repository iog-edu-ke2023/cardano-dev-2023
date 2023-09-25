{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Power where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.List                  (find)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Numeric.Natural            (Natural)

naivePower :: Natural -> Integer -> Integer
naivePower n x
    | n == 0    = 1
    | otherwise = x * naivePower (n - 1) x

power :: Natural -> Integer -> Integer
power n x
    | n == 0    = 1
    | even n    = let y = power (n `div` 2) x in y * y -- x ^ (2m) = (x ^ m) ^ 2
    | otherwise = x * power (n - 1) x

power10 :: Integer -> Integer
power10 x =
  let
    p10 = p5 * p5
    p5  = x * p2 * p2
    p2  = x * x
  in
    p10

exp1 :: Int
exp1 = 1 + 1

exp2 :: CodeQ Int
exp2 = [|| 1 + 1 ||]

exp3 :: CodeQ Int
exp3 = [|| $$exp2 * $$exp2 ||] -- (1 + 1) * (1 + 1), NOT 1 + 1 * 1 + 1

exp4 :: CodeQ Int
exp4 = [|| let x = $$exp2 in x * x ||]

mul :: CodeQ Int -> CodeQ Int
mul x = [|| $$x * $$x ||]

mul' :: CodeQ Int -> CodeQ Int
mul' x = [|| let y = $$x in y * y ||]

exp7 :: CodeQ Int
exp7 = mul' $ mul' exp2

naivePower' :: Natural -> CodeQ Integer -> CodeQ Integer
naivePower' 0 _ = [|| 1 ||]
naivePower' n x = [|| $$x * $$(naivePower' (n - 1) x) ||]

spower :: Natural -> CodeQ Integer -> CodeQ Integer
spower 0 _      = [|| 1 ||]
spower n x
    | even n    = [|| let y = $$(spower (n `div` 2) x) in y * y ||]
    | otherwise = [|| $$x * $$(spower (n - 1) x) ||]

-- (CodeQ a -> CodeQ b) =? CodeQ (a -> b)

to :: (CodeQ a -> CodeQ b) -> CodeQ (a -> b)
to f = [|| \a -> $$(f [|| a ||]) ||]

from :: CodeQ (a -> b) -> CodeQ a -> CodeQ b
from f a = [|| $$f $$a ||]

-- strange :: CodeQ (CodeQ Int -> Int)
-- strange = [|| \x -> 1 + $$x ||]

persist :: Int -> CodeQ Int
persist n = [|| 3 * $$(liftTyped n) ||] -- [|| 3 * n ||]

persist' :: Lift a => a -> CodeQ a
persist' a = [|| a ||]

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq, Ord)

instance Lift a => Lift (Tree a) where

    liftTyped :: (Lift a, Quote m) => Tree a -> Code m (Tree a)
    liftTyped (Leaf x)   = [|| Leaf $$(liftTyped x) ||]
    liftTyped (Node l r) = [|| Node $$(liftTyped l) $$(liftTyped r) ||]

exTree :: Tree Bool
exTree = Node (Leaf True) $ Node (Leaf False) (Leaf True)

foo :: Int -> String
foo n = show n <> show n

bar :: CodeQ String
bar = [|| foo 42 ||]

fib :: Natural -> Natural
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibManual :: Natural -> Natural
fibManual n = case find (\p -> fst p == n) table of
    Just (_, x) -> x
    Nothing     -> fibManual (n - 2) + fibManual (n - 1)
  where
    table = [(0, 1), (1, 1), (2, 2), (3, 3), (4, 5), (5, 8)]

smemo :: forall a b. (Eq a, Lift a, Lift b) => [a] -> (a -> b) -> CodeQ (a -> b) -> CodeQ (a -> b)
smemo xs f c =
    [|| \x -> case find (\p -> fst p == x) table of
                Just (_, y) -> y
                Nothing     -> $$c x
        ||]
  where
    table :: [(a, b)]
    table = [(x, f x) | x <- xs]


exp2' :: Q Exp
exp2' = [| 1 + 1 |]

exp3' :: Q Exp
exp3' = [| $exp2' * $exp2' |]

te :: Q Exp
te = [| head True |]

myFsts :: [Int] -> Q [Dec]
myFsts = mapM myFst

myFst :: Int -> Q Dec
myFst n = do
    let f = mkName $ "fst_" <> show n
    x <- newName "x"
    let p = pat x
    pure $ FunD f [Clause [p] (NormalB $ VarE x) []]

  where
    pat :: Name -> Pat
    pat x = TupP $ VarP x : replicate (n - 1) WildP

{-
ex3 :: Q Exp
ex3 = [| \(x, _, _, _) -> x |]
-}

showExp :: Q Exp -> IO ()
showExp e = runQ $ do
    x <- e
    liftIO $ print x

showNameInfo :: Name -> Q Exp
showNameInfo n = runQ $ do
    info <- reify n
    let s = show info
    [| s |]
