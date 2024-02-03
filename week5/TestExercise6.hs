{- vim: set colorcolumn=80: -}

-- Test calls for exercise 6. 

module TestExercise6 where

import Calc
import qualified Data.Map as M

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

-- :t add (lit 3) (var "x")

exp0 = withVars [("x", 6)] $ add (lit 3) (var "x")
exp1 = withVars [("x", 6)] $ add (lit 3) (var "y")
exp2 = withVars [("x", 6), ("y", 3)]
    $ mul (var "x") (add (var "y") (var "x"))
