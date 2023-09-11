module FreeMonads.Expr where

import           Data.Map (Map)

data Expr =
      Lit Int
    | Add Expr Expr
    | Div Expr Expr
    | Var String
    | Seq Expr Expr
    | Assign String Expr
    deriving (Show, Eq, Ord)

program :: Expr
program =
          Assign "x" (Lit 16)                -- x := 16;
    `Seq` Assign "x" (Div (Var "x") (Lit 2)) -- x := x / 2;
    `Seq` Add (Var "x") (Lit 1)              -- x + 1;

type Env = Map String Int
