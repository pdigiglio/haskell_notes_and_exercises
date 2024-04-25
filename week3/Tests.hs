module Tests where

import Golf
import Test.HUnit

test_skips :: Test
test_skips =
  "skips"
    ~: TestList
      [ skips "ABCD" ~?= ["ABCD", "BD", "C", "D"],
        skips "hello!" ~?= ["hello!", "el!", "l!", "l", "o", "!"],
        skips ([1] :: [Int]) ~?= [[1]],
        skips [True, False] ~?= [[True, False], [False]],
        skips ([] :: [Int]) ~?= []
      ]

-- TODO:
-- Property check for skips: size of output is same as size of input

test_localMaxima :: Test
test_localMaxima =
  "local maxima"
    ~: TestList
      [ localMaxima [2, 9, 5, 6, 1] ~?= [9, 6],
        localMaxima [2, 3, 4, 1, 5] ~?= [4],
        localMaxima [1, 2, 3, 4, 5] ~?= []
      ]

test_histo :: Test
test_histo =
  "histo"
    ~: TestList
      [ histogram [1, 1, 1, 5] ~?= h1,
        histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9] ~?= h2
      ]
  where
    h1 =   " *        \n"
        ++ " *        \n"
        ++ " *   *    \n"
        ++ "==========\n"
        ++ "0123456789\n"
    h2 =   "    *     \n"
        ++ "    *     \n"
        ++ "    * *   \n"
        ++ " ******  *\n"
        ++ "==========\n"
        ++ "0123456789\n"

main :: IO ()
main =
  runTestTTAndExit $
    TestList
      [ test_skips,
        test_localMaxima,
        test_histo
      ]
