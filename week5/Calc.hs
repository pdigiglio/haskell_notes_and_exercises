{- vim: set colorcolumn=80: -}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
--import qualified StackVM as S
import qualified Data.Map as M

{- Exercise 1 -}

{- |
Evaluate an expression.

> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
-}
eval :: ExprT -> Integer
eval e = case e of
    (Lit x)   -> x
    (Add l r) -> (eval l) + (eval r)
    (Mul l r) -> (eval l) * (eval r)

{- Exercise 2 -}
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
    (Just x) -> Just $ eval x
    _        -> Nothing

{- Exercise 3 -}
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

{- Exercise 4 -}
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq,Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax l) (MinMax r) = MinMax (max l r)
    mul (MinMax l) (MinMax r) = MinMax (min l r)

newtype Mod7 = Mod7 Integer deriving (Eq,Show)

mod7 :: Integer -> Mod7
mod7 = Mod7 . (`mod` 7)

instance Expr Mod7 where
    lit = mod7
    add (Mod7 l) (Mod7 r) = mod7 (l + r)
    mul (Mod7 l) (Mod7 r) = mod7 (l * r)

{- Exercise 5 -}
--instance Expr S.Program where
--    lit i = [S.PushI i]
--    add l r = case (stackVM l, stackVM r) of
--        (Right lExp, Right rExp) -> [lExpr, rExpr, S.Add]
--        _                        -> []
--    mul l r = case (stackVM l, stackVM r) of
--        (Right lExp, Right rExp) -> [lExpr, rExpr, S.Mul]
--        _                        -> []
--
--    
--compile :: String -> Maybe S.Program
--compile = parseExp lit add mul str 
--

{- Exercise 6 -}

class HasVars a where
    -- | Retrieve the value of a named variable.
    var :: String -> a

{- |
A data type that is either a named variable or (something like) @ExprT@.

Questions:
 
  1. What's the point of this type? I never use it.
  2. Can't I define @VarExprT@ as the sum type of @Var String@ and @ExprT@?
-}
data VarExprT =
      Var String
    | Lit_ Integer 
    | Add_ VarExprT VarExprT
    | Mul_ VarExprT VarExprT
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit_
    add = Add_
    mul = Mul_

instance HasVars VarExprT where
    var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
    -- | A literal is just the value I get passed in. I can discard the map.
    lit = const . Just
    add l r = (\m -> case (l m, r m) of
        (Just lx, Just rx) -> Just $ lx + rx
        _                  -> Nothing)
    mul l r = (\m -> case (l m, r m) of
        (Just lx, Just rx) -> Just $ lx * rx
        _                  -> Nothing)

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup
--    var x = (\m -> m M.!? x)
    
