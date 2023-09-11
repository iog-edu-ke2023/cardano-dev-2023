{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}

module FreeMonads.Free where

import           Control.Monad   (ap, liftM)
import           FreeMonads.Expr
import qualified FreeMonads.MTL  as MTL

data EvalOp b =
      DivByZeroError
    | UnknownVar String
    | VarLookup String (Int -> b)
    | VarSet String Int b
    deriving Functor

data GPOp b =
      Get (Int -> b)
    | Put Int b
    deriving Functor

data Free f a =
      Pure a
    | Free (f (Free f a))

type Eval = Free EvalOp
type GP = Free GPOp

instance Functor f => Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap = liftM

instance Functor f => Applicative (Free f) where
    pure :: a -> Free f a
    pure = Pure

    (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    (<*>) = ap

instance Functor f => Monad (Free f) where
    return :: a -> Free f a
    return = pure

    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    Pure x >>= k = k x
    Free f >>= k = Free $ (>>= k) <$> f
    -- f            :: f (Free f a)
    -- k            :: a -> Free f b
    -- (>>= k)      :: Free f a -> Free f b
    -- fmap (>>= k) :: f (Free f a) -> f (Free f b)

fromEval :: Eval a -> MTL.Eval a
fromEval (Pure a)               = pure a
fromEval (Free DivByZeroError)  = MTL.divByZeroError
fromEval (Free (UnknownVar s))  = MTL.unknownVar s
fromEval (Free (VarLookup s k)) = MTL.varLookup s >>= fromEval . k
fromEval (Free (VarSet s n k))  = MTL.varSet s n >> fromEval k

divByZeroError :: Eval a
divByZeroError = Free DivByZeroError

unknownVar :: String -> Eval a
unknownVar s = Free (UnknownVar s)

varLookup :: String -> Eval Int
varLookup s = Free (VarLookup s pure)

varSet :: String -> Int -> Eval ()
varSet s n = Free (VarSet s n $ pure ())

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
