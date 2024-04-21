{-# OPTIONS_GHC -Wall #-}

import Data.List
import Log
import LogAnalysis
import Test.HUnit

test_parseError :: Test
test_parseError =
  TestList
    [ "Parse error msg"
        ~: parseMessage "E 2 562 help help"
        ~?= LogMessage (Error 2) 562 "help help",
      "Parse error msg (empty)"
        ~: parseMessage "E 2 562"
        ~?= LogMessage (Error 2) 562 ""
    ]

test_parseWarning :: Test
test_parseWarning =
  TestList
    [ "Parse warn msg"
        ~: parseMessage "W 29 la la la"
        ~?= LogMessage Warning 29 "la la la",
      "Parse warn msg (empty)"
        ~: parseMessage "W 29"
        ~?= LogMessage Warning 29 ""
    ]

test_parseInfo :: Test
test_parseInfo =
  TestList
    [ "Parse info msg"
        ~: parseMessage "I 29 la la la"
        ~?= LogMessage Info 29 "la la la",
      "Parse info msg (empty)"
        ~: parseMessage "I 29"
        ~?= LogMessage Info 29 ""
    ]

test_parseUnknown :: Test
test_parseUnknown =
  TestList
    [ "Parse unknown msg"
        ~: parseMessage "This is not in the right format"
        ~?= Unknown "This is not in the right format",
      "Parse unknown msg (empty)"
        ~: parseMessage ""
        ~?= Unknown ""
    ]

sample :: [String]
sample =
  [ "I 6 Completed armadillo processing",
    "I 1 Nothing to report",
    "I 4 Everything normal",
    "I 11 Initiating self-destruct sequence",
    "E 70 3 Way too many pickles",
    "E 65 8 Bad pickle-flange interaction detected",
    "W 5 Flange is due for a check-up",
    "I 7 Out for lunch, back in two time steps",
    "E 20 2 Too many pickles",
    "I 9 Back from lunch",
    "E 99 10 Flange failed!"
  ]

expected :: [String]
expected =
  [ "Way too many pickles",
    "Bad pickle-flange interaction detected",
    "Flange failed!"
  ]

test_whatWentWrong :: Test
test_whatWentWrong =
  "whatWentWrong in sample"
    ~: filterSample
    ~?= expected
  where
    filterSample = whatWentWrong $ map parseMessage sample

test_sorted :: Test
test_sorted =
  "sorted"
    ~: sortWithTree messages
  ~?= sortOnTimestamp (filterOutUnknown messages)
  where
    messages = map parseMessage sample
    sortWithTree = inOrder . build
    sortOnTimestamp = sortOn timeStamp
    timeStamp (LogMessage _ ts _) = ts
    timeStamp _ = undefined
    isUnknown (Unknown _) = True
    isUnknown _ = False
    filterOutUnknown = filter (not . isUnknown)

testList :: Test
testList =
  TestList
    [ test_parseError,
      test_parseWarning,
      test_parseInfo,
      test_parseUnknown,
      test_whatWentWrong,
      test_sorted
    ]

main :: IO ()
main = runTestTTAndExit testList
