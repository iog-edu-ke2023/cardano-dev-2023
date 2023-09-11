{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}

module FreeMonads.AdHoc where

import           Control.Monad   (ap, liftM)
import           FreeMonads.Expr
import qualified FreeMonads.MTL  as MTL

data Eval a =
      Pure a
    | DivByZeroError
    | UnknownVar String
    | VarLookup String (Int -> Eval a)
    | VarSet String Int (Eval a)

fromEval :: Eval a -> MTL.Eval a
fromEval (Pure a)        = pure a
fromEval DivByZeroError  = MTL.divByZeroError
fromEval (UnknownVar s)  = MTL.unknownVar s
fromEval (VarLookup s k) = MTL.varLookup s >>= fromEval . k
fromEval (VarSet s n k)  = MTL.varSet s n >> fromEval k

divByZeroError :: Eval a
divByZeroError = DivByZeroError

unknownVar :: String -> Eval a
unknownVar = UnknownVar

varLookup :: String -> Eval Int
varLookup s = VarLookup s pure

varSet :: String -> Int -> Eval ()
varSet s n = VarSet s n $ pure ()

instance Functor Eval where
    fmap :: (a -> b) -> Eval a -> Eval b
    fmap = liftM

instance Applicative Eval where
    pure :: a -> Eval a
    pure = Pure

    (<*>) :: Eval (a -> b) -> Eval a -> Eval b
    (<*>) = ap

instance Monad Eval where
    return :: a -> Eval a
    return = pure

    (>>=) :: Eval a -> (a -> Eval b) -> Eval b
    Pure x         >>= k  = k x
    DivByZeroError >>= _  = DivByZeroError
    UnknownVar s   >>= _  = UnknownVar s
    VarLookup s k  >>= k' = VarLookup s $ \n -> k n >>= k'
    VarSet s n k   >>= k' = VarSet s n $ k >>= k'

{-
data Tree a where
    Leaf :: a -> Tree a
    Node :: Tree a -> Tree a -> Tree a

data List a where
    Nil  :: List a
    Cons :: a -> List a -> List a
-}


eval :: Expr -> Eval Int
eval (Lit n)      = pure n
eval (Add x y)    = (+) <$> eval x <*> eval y
eval (Div x y)    = do
    n <- eval y
    if n == 0
        then divByZeroError
        else do
            m <- eval x
            pure $ div m n
eval (Var s)      = varLookup s
eval (Seq x y)    = eval x >> eval y
eval (Assign s x) = do
    n <- eval x
    varSet s n
    pure n

test :: Either String Int
test = MTL.runEval $ fromEval $ eval program
