--module Example where

import Data.List

main :: IO()
main =
    do
    print $ everyOtherNth (-2) "0123456789"
    print $ skips  "0123456789"
    print $ localMaxima [2,9,5,6,1]
    print $ localMaxima [2,3,4,1,5]
    print $ localMaxima [1,2,3,4,5]
    print $ transpose ["a", "bc"]
    print $ freqs  [1, 2, 3, 1]
    print $ groups [1, 2, 3, 1]


everyOtherNth ::  Int -> [a] -> [a]
everyOtherNth k l  =
    [c | (i, c) <- zip [1..] l, i `mod` k == 0]

skips :: [a] -> [[a]]
skips l = [ [c | (i, c) <- zip [1..] l, mod i k == 0] | k <- [1..(length l)] ]

localMaxima :: [Integer] -> [Integer]
localMaxima l = [ y | (x,y,z) <- zip3 l (drop 1 l) (drop 2 l), x < y && z < y]

--transpose
-- map (take 5 . (++ repeat ' ') . map fst)

--histo :: [Int] -> [[Char]]
--histo =
--    map ( map fst)
--    . group
--    . zip (repeat '*')
--    . sort

count :: Eq a => a -> [a] -> Int
count i = length . filter (==i)

freqs :: [Int] -> [Int]
freqs = zipWith count [0..9] . repeat

groups :: [Int] -> [[Int]]
groups = group . sort . (++ [0..9])