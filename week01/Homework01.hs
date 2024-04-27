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
hanoi n a b c
  | n <= 0 = []
  | n == 1 = [(a, b)]
  | otherwise =
      hanoi (n - 1) a c b
        ++ hanoi 1 a b c
        ++ hanoi (n - 1) c b a

{- Exercise 6 -}

-- | Return a list of moves to be performed to move the stack of discs from the
-- first peg to the second using the third and the fourth as temporary buffers.
hanoi4Impl ::
  -- | Customization point: return the number of disks to move to the temporary
  -- peg at each step.
  (Integer -> Integer) ->
  -- | The number of disks in the stack
  Integer ->
  -- | The label for the first peg
  Peg ->
  -- | The label for the second peg
  Peg ->
  -- | The label for the third peg
  Peg ->
  -- | The label for the fourth peg
  Peg ->
  -- | The list of moves
  [Move]
hanoi4Impl topStackSize n a b c d
  | n <= 0 = []
  | n == 1 = [(a, b)]
  | n == 2 = [(a, d), (a, b), (d, b)]
  | n == 3 = [(a, d), (a, c), (d, c), (c, b), (d, b)]
  | otherwise =
      let k = topStackSize n
       in hanoi4Impl topStackSize k a c b d -- Move top k to peg c
            ++ hanoi (n - k) a b d -- Can't use peg c here
            ++ hanoi4Impl topStackSize k c b a d -- Move back top k from c to b

-- | Return a list of moves to be performed to move the stack of discs from the
-- first peg to the second using the third and the fourth as temporary buffers.
hanoi4Naive ::
  -- | The number of disks in the stack
  Integer ->
  -- | The label for the first peg
  Peg ->
  -- | The label for the second peg
  Peg ->
  -- | The label for the third peg
  Peg ->
  -- | The label for the fourth peg
  Peg ->
  -- | The list of moves
  [Move]
hanoi4Naive = hanoi4Impl (\n -> n - 2)

-- | Return a list of moves to be performed to move the stack of discs from the
-- first peg to the second using the third and the fourth as temporary buffers.
hanoi4Optim ::
  -- | The number of disks in the stack
  Integer ->
  -- | The label for the first peg
  Peg ->
  -- | The label for the second peg
  Peg ->
  -- | The label for the third peg
  Peg ->
  -- | The label for the fourth peg
  Peg ->
  -- | The list of moves
  [Move]
hanoi4Optim = hanoi4Impl topStackSize
  where
    -- Optimal solution: see https://en.wikipedia.org/wiki/Tower_of_Hanoi
    topStackSize y =
      let x = intToDouble (2 * y + 1)
          r = sqrt x
       in y + 1 - round r
    intToDouble = fromIntegral :: Integer -> Double
