{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List
--import Data.List.Split (chunksOf)

--main :: IO()
--main = do
--    print $ skips_ "0123456789"
--    print $ skips_ "ABCD"
--    print $ skips_ "Hello!"
--    print $ skips_ [1]
--    print $ skips_ [True, False]
--    --print $ skips []
--    --print $ test 2  "0123456789"

chunk :: Int -> [a] -> [[a]]
chunk _ []   = []
chunk n l 
    | n <= 0    = chunk 0 l
    | otherwise = ((take n l) : (chunk n $ drop n l))

--chunk :: Int -> [a] -> [[a]]
--skips n list =
--
--    $ zip s list
--    where s = map (`div` n) [0..]
--    foldr (:) []
--    . map snd
--    . filter (\(i, _) -> 0 == mod i 2)
--    . zip [0..]

--takeEvery :: Int -> [a] -> [a]
--takeEvery 0 l = l
--takeEvery n l =
--    foldr (:) []
--    $ map snd
--    $ filter (\(i, _) -> 0 == mod i n)
--    $ zip [0..] l
--
--skips :: [a] -> [[a]]
--skips i = map (\n -> takeEvery n i) [1 .. length i]

--skips :: [a] -> [[a]]
--skips l = map (\n -> takeEvery n i) [1 .. length i]

-- ATTEMPT #0 (on Compiler Explorer, which doesn't have `chunk`).
--
-- | 'everyNth' @k l@ selects every element from @l@ whose position is a
-- multiple of @k@.
-- If @k@ is greater than the number of elements in @l@, return an empty list.
-- If @k@ is smaller or equal to 0, return the whole list.
everyNth :: Int -> [a] -> [a]
everyNth k l
    | k <= 0 = l
    | True = map (head . snd) $ zipWith splitAt [0, k..n] (repeat l)
    where n = length l - 1

skips' :: [a] -> [[a]]
skips' =
    filter ((/= 0) . length) -- Filter-out empty lists
    . zipWith everyNth [1..] 
    . tails                  -- Evaluate the tails of the list

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
    

-- ATTEMPT #1 (with `chunk`)
--
-- The `map` maps a seqence of integers, @k@, from 1 to the length of @l@. For
-- each @k@. It does the following:
--
--  * chunk :: Int -> [a] -> [[a]]
--    `chunk k l` will transform @l@ into a list of sub-lists of length @k@
--    (except for the last one, which may be shorter).
--
--  * filter ((==k) . length) :: [[a]] -> [[a]]
--    Removes all the sub-lists whose length is not @k@. I do this to remove
--    the last sub-list when the length of @l@ is not a multiple of @k@.
--
--  * map last :: [[a]] -> [a]
--    Takes the last element of every sub-list, i.e. every other k-th element
--    in @l@. If I didn't do the filter above, I'd get every other k-th element
--    AND the last element. 
--
skips_ :: [a] -> [[a]]
skips_ l = map (everyOtherNth_ l) [1 ..  (length l)]

everyOtherNth_ :: [a] -> Int -> [a]
everyOtherNth_ l n = map last $ filter ((==n) . length) $ chunk n l




--skips s = map everyNth [1..strLen] 
--    where
--        strLen = length s - 1
--        eveyNth k = map (head . snd) $ zipWith splitAt [0, k..strLen] (repeat s)


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
