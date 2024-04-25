{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List


{- Exercise 1 -}

-- | Return a list of lists:
--
--  * The first list in the output is the same as the input;
--  * The second one contains every second element from the input;
--  * ... 
--  * The n-th one contains every n-th element from the input list.
--
-- /Solution:/
-- 
-- Map each @n@ in range @[1 .. length l]@ to a list in the output. The mapping
-- function performs the following steps: 
--
--  1. zip every element in the input list with its 1-based index;
--  2. take elements whose index is a multiple of @n@.
--
-- (Note: Solution (*) is the same as this but doesn't use list comprehension.
-- At this point, (*) needs to discard the index).
--
-- /Alternative solution:/
-- 
-- Map each @n@ in range @[1 .. length l]@ to a list in the output. The mapping
-- function performs the following steps: 
--
--  1. Split the list in sublists (chunks) of length @n@;
--  2. Possibly remove the last element, that may be shorter than @n@.
--  3. Take the last element of each remaining chunk.
skips :: [a] -> [[a]]
skips l = [[c | (i, c) <- zip [1 ..] l, mod i n == 0] | n <- [1 .. length l]]
--
-- (*) Same as:
-- skips l = [g n l | n <- r]
--  where
--    -- Get every n-th element from a given list.
--    g n =
--      map snd
--        . filter (\(i, _) -> 0 == mod i n)
--        . zip r
--    r = [1 .. length l]
--
-- Alternative solution:
-- skips l = [g n l | n <- r]
--   where
--     -- Get every n-th element from a given list.
--     g n =
--       map last
--         . filter ((== n) . length)
--         . chunksOf n
--     r = [1 .. length l]


{- Exercise 2 -}

-- | This function finds all the local maxima in the input list and returns
-- them in order. A local maximum of a list is an element of the list which is
-- strictly greater than both the elements immediately before and after it.
--
-- /Solution:/
--
--  1. Obtain the triplets in list @l@:
--
--     - Evaluate @l' = drop 1 l@ containing all but the first element of @l@
--     - Evaluate @l'' = drop 2 l@ containing all but the first two elements
--       of @l@.
--     - zipping l, l', and l'' together.
--
--  2. Filter out all the triplets whose center element is not a local maximum.
localMaxima :: [Integer] -> [Integer]
localMaxima l = [y | (x, y, z) <- zip3 l (drop 1 l) (drop 2 l), y > x, y > z]


{- Exercise 3 -}

-- | This function takes a list of integers in @[0..9]@ and outputs a vertical
-- histogram showing how many of each number were in the input list.
--
-- /Solution:/
--  
--  *. Count the occurrences of 0,1,...,9 in the list.
--
--  *. Find the maximum occurrence, @m@;
--
--  *. For each @x@ in 0,1,...9, get its frequency, @f@, and map it to string:
--
-- >              f times '*'
-- >              v 
-- > "            ******"
-- >  ^
-- >  (m - f) times ' '
--
--  *. Transpose the strings above to arrange them vertically;
--
--  *. Concatenate the bottom parts of the histogram (namely, the horizontal 
--  bar @replicate 10 '='@, and the labels).
histogram :: [Integer] -> String
histogram l = unlines $ transpose s ++ [r 10 '=', ['0' .. '9']]
  where
    -- Count the elements in a list that equal @k@.
    c k = length . filter (== k)
    -- Evaluate the occurrences of [0,1,...,9] in the list. 
    f = map (`c` l) [0 .. 9]
    -- Alias for @replicate@.
    r = replicate
    -- Map the frequencies to (horizontal) histogram strings.
    s = map (\k -> r (maximum f - k) ' ' ++ r k '*') f
