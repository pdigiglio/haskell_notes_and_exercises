{-# OPTIONS_GHC -Wall #-}

module Homework01 where

{- Exercise 1 -}

-- | Given a positive integer, return the list of its digits (as integers) in
-- reverse order.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = digit : toDigitsRev rest
  where
    digit = n `mod` 10
    rest = n `div` 10

-- | Given a positive integer, return the list of its digits (as integers).
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits rest ++ [digit]
  where
    digit = n `mod` 10
    rest = n `div` 10
-- Or:
-- toDigits = reverse . toDigitsRev

{- Exercise 2 -}

-- | Double every other number /from the right/ (that is, the second-to-last,
-- fourth-to-last, ... numbers are doubled).
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [i] = [i]
doubleEveryOther (f : s : rest)
  | odd (length rest) = f : doubleEveryOther (s : rest)
  | otherwise = (f + f) : s : doubleEveryOther rest

{- Exercise 3 -}

-- | Sum all the digits of the given list. If any integer in the list has more
-- than one digit, its single digits are summed.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [f]
  | f < 10 = f
  | otherwise = sumDigits $ toDigitsRev f
sumDigits (f : rest) = sumDigits [f] + sumDigits rest

{- Exercise 4 -}

-- | Return whether the input @Integer@ could be a valid credit-card number.
validate :: Integer -> Bool
validate = (== 0) . mod10 . sumDigits . doubleEveryOther . toDigits
  where
    mod10 = \x -> mod x 10

{- Exercise 5 -}

-- | The peg label.
type Peg = String

-- | The move of a disk from one peg to another.
type Move = (Peg, Peg)

-- | Return a list of moves to be performed to move the stack of discs from the
-- first peg to the second using the third as a temporary buffer.
hanoi ::
  -- | The number of disks in the stack
  Integer ->
  -- | The label for the first peg
  Peg ->
  -- | The label for the second peg
  Peg ->
  -- | The label for the third peg
  Peg ->
  -- | The list of moves
  [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c
  | n <= 0 = []
  | otherwise =
      hanoi (n - 1) a c b
        ++ hanoi 1 a b c
        ++ hanoi (n - 1) c b a
