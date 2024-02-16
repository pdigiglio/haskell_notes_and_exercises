{-# OPTIONS_GHC -Wall #-}

module Week4.Homework where

import Data.List
import qualified Data.List.NonEmpty as NE (group, head, length)

{- Ex. 1: Wholemail programming -}

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- |
-- @fun1' list@ does the following:
--
--   - Removes odd numbers from the list;
--   - Subtracts 2 from each element in the list;
--   - Multiplies all the remnaining numbers.
--
-- If the list is empty, returns 1.
fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- Return the number following `n` in the Hailstone sequence.
nextHailstone :: Integral a => a -> a
nextHailstone n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

-- Return the number following `n` in the Hailstone sequence.
hailstoneSeq :: Integral a => a -> [a]
hailstoneSeq = takeWhile (/= 1) . iterate nextHailstone

-- |
-- @fun' n@ does the following:
--
--  - Generates the Hailstone sequence for @n@;
--  - Removes the odd numbers from the sequence;
--  - Reduces the sequence to its sum.
fun2' :: Integer -> Integer
fun2' = sum . filter even . hailstoneSeq

{- Ex. 2: Folding with trees -}
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- |
-- Returns whether a tree is balanced.
--
-- The tree is balanced if the heights of its left and right subtrees differ by at
-- most 1 and if both its left and right subtrees are balanced.
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l _ r) =
  abs (height l - height r) <= 1
    && isBalanced l
    && isBalanced r

-- | Returns the height of a tree.
height :: Tree a -> Integer
height (Node h _ _ _) = h
height _ = -1

-- |
-- Make a new node.
--
--
-- @makeTree l val r@ creates a node containing @val@ and whose children are @l@
-- and @r@. The height of the node is evaluated by the height of the children.
makeNode :: Tree a -> a -> Tree a -> Tree a
makeNode left val right = Node h left val right
  where
    h = 1 + max (height left) (height right)

-- | Insert a new node in the tree, keeping the tree balanced.
insertNode :: a -> Tree a -> Tree a
insertNode val Leaf = makeNode Leaf val Leaf
insertNode val (Node _ l v r)
  | height l <= height r = makeNode (insertNode val l) v r
  | otherwise = makeNode l v (insertNode val r)

-- | Given a list of @a@'s, map it to a balanced @Tree a@.
foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

{- Ex. 3: Mode folds! -}
{- Ex. 3.1 -}

-- | Get a list of @Bool@'s and reduce them with a XOR.
xor :: [Bool] -> Bool
xor = odd . foldl' (+) 0 . map fromEnum

-- The above cam be implemented without an explicit fold:
--xor = odd . sum . map fromEnum

-- Or with a single fold:
--xor = foldl' (/=) False

{- Ex. 3.2 -}

-- (:) :: a -> [a] -> [a]
-- (.) :: (c -> d) -> (b -> c) -> b -> d
--
-- => ((:) .)
-- => c = a
-- => d = [a] -> [a]
--
-- ((:) .) :: (b -> a) -> b -> ([a] -> [a])
--         :: (b -> a) -> b -> [a] -> [a]
-- ((:) .) f x xs = ((:) . f) x xs
--                = ((:) . f) x xs
--                = ((:) (f x)) xs
--                = ((f x) : xs)

-- | Implement a map using @foldr@.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

{- Ex. 3.3 -}

-- | Implement a left fold using @foldr@.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

{- Ex 4: Finding primes -}

-- |
-- @sieveSundaram n@ returns all the odd prime numbers up to @2n+1@.
-- It uses the Sundaram sieve algorithm.
--
-- The idea is that I merge the list @[1..n]@ with the sieves up to @n@. Then I
-- remove the duplicates and apply @2*x + 1@ to the remaining elements.
sieveSundaram :: Integral a => a -> [a]
sieveSundaram n =
  map ((+ 1) . (* 2) . NE.head)
    . filter ((== 1) . NE.length)
    . NE.group
    . sort
    $ [1 .. n] ++ sundaramSieveList n

-- |
-- Generate the list of sieves for the Sundaram algorithm up to @n@.
--
-- Use list comprehension to
--
--   1. Generate all the pairs @i,j@ such that @j=1..n, i=1..j@
--   2. Evaluate a sieve from @i,j@
--
-- Then, take only the sieves that are no greater than @n@.
sundaramSieveList :: Integral a => a -> [a]
sundaramSieveList n = filter (<= n) [sieve i j | j <- [1 .. n], i <- [1 .. j]]
  where
    sieve i j = i + j + 2 * i * j
