{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FreeMonads.MTL
    ( Eval
    , divByZeroError
    , unknownVar
    , runEval
    , varLookup
    , varSet
    , eval
    ) where

import           Control.Monad.Except   (ExceptT, MonadError (..), runExceptT)
import           Control.Monad.Identity (Identity (..))
import           Control.Monad.State    (MonadState (..), StateT (..),
                                         evalStateT, modify)
import qualified Data.Map               as Map

import           FreeMonads.Expr

newtype Eval a = Eval (StateT Env (ExceptT String Identity) a)
    deriving (Functor, Applicative, Monad, MonadError String, MonadState Env)

divByZeroError :: Eval a
divByZeroError = throwError "division by 0"

unknownVar :: String -> Eval a
unknownVar s = throwError $ "unknown variable: " <> s

varLookup :: String -> Eval Int
varLookup s = do
    env <- get
    case Map.lookup s env of
        Just n  -> pure n
        Nothing -> unknownVar s

varSet :: String -> Int -> Eval ()
varSet s n = modify $ Map.insert s n

runEval :: Eval a -> Either String a
runEval (Eval m) = runIdentity $ runExceptT $ evalStateT m mempty


{-
eval :: Expr -> Identity Int
eval (Lit n)   = pure n
eval (Add x y) = (+) <$> eval x <*> eval y
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
