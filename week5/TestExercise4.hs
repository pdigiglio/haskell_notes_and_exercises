{- vim: set colorcolumn=80: -}

-- Test calls for exercise 4. 

module TestExercise4 where

import Calc
import Parser

test :: Expr a => String -> Maybe a
test = parseExp lit add mul 

testStr :: String
testStr = "(3 * -4) + 5"

testExp :: Expr a => Maybe a
testExp = test testStr

testInteger :: Maybe Integer
testInteger = testExp 

testBool :: Maybe Bool
testBool = testExp 

testMM :: Maybe MinMax
testMM = testExp 

testSat :: Maybe Mod7
testSat = testExp
