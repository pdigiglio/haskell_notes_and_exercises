{-# OPTIONS_GHC -Wall #-}

import Homework01
import Test.HUnit

test_toDigits :: Test
test_toDigits =
  TestList
    [ "parse number" ~: toDigits 1234 ~?= [1, 2, 3, 4],
      "parse zero" ~: toDigits 0 ~?= [],
      "parse negative" ~: toDigits (-17) ~?= []
    ]

test_toDigitsRev :: Test
test_toDigitsRev =
  TestList
    [ "parse number" ~: toDigitsRev 1234 ~?= [4, 3, 2, 1],
      "parse zero" ~: toDigitsRev 0 ~?= [],
      "parse negative" ~: toDigitsRev (-17) ~?= []
    ]

-- TODO: property: toDigit = reverse . toDigitRev

test_doubleEveryOther :: Test
test_doubleEveryOther =
  TestList
    [ "even list" ~: doubleEveryOther [8, 7, 6, 5] ~?= [16, 7, 12, 5],
      "odd list" ~: doubleEveryOther [1, 2, 3] ~?= [1, 4, 3]
    ]

test_sumDigits :: Test
test_sumDigits = sumDigits [16, 7, 12, 5] ~?= 22 -- 1 + 6 + 7 + 1 + 2 + 5

test_validate :: Test
test_validate =
  TestList
    [ "valid number" ~: validate 4012888888881881 ~?= True,
      "invalid number" ~: validate 4012888888881882 ~?= False
    ]

test_hanoi :: Test
test_hanoi =
  TestList
    [ hanoi 2 "a" "b" "c" ~?= [("a", "c"), ("a", "b"), ("c", "b")]
    ]

main :: IO ()
main =
  runTestTTAndExit $
    TestList
      [ test_toDigits,
        test_toDigitsRev,
        test_doubleEveryOther,
        test_sumDigits,
        test_validate,
        test_hanoi
      ]
