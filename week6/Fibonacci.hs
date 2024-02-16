{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

{- Exercise 1 -}

-- |
-- Recursive implementation of Fibonacci numbers:
--
-- > F(0) = 0
-- > F(1) = 1
-- > F(n) = F(n-1) + F(n-2)    [n > 1]
--
-- This function is total: it will properly work for negative inputs too.
fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n
  | n > 0 = fib (n - 1) + fib (n - 2)
  | otherwise = (if even n then -1 else 1) * fib (-n)

-- | Generate an infinite list of Fibonacci numbers using 'fib'.
fibs1 :: Integral a => [a]
fibs1 = map fib [0 ..]

{- Exercise 2 -}

-- |
-- Generate an infinite list of Fibonacci numbers.
--
-- Computing the first /n/ numbers costs /O(n)/.
fibs2 :: Integral a => [a]
fibs2 =
  map fst
    . iterate (\(n2, n1) -> (n1, n1 + n2))
    $ (0, 1)

-- |
-- Another implementation of the Fibonacci sequence.
--
-- You can get an intuition on how this works like this:
--
--
-- > zip  _  0   1   x   x   x  ...  <- fibs
-- >     \_  1   x   x   x  ...      <- tail fibs
-- >         +   +   +   +
-- > 0   1   x   x   x   x
-- >
-- > zip  _  0   1   x   x   x  ...  <- fibs
-- >     \_  1   x   x   x  ...      <- tail fibs
-- >         +   +   +   +
-- > 0   1  (1)  x   x   x
-- >
-- > zip  _  0   1  (1)  x   x  ...  <- fibs
-- >     \_  1  (1)  x   x  ...      <- tail fibs
-- >         +   +   +   +
-- > 0   1  (1)  x   x   x
-- >
-- > zip  _  0   1  (1)  x   x  ...  <- fibs
-- >     \_  1  (1)  x   x  ...      <- tail fibs
-- >         +   +   +   +
-- > 0   1  (1) (2)  x   x
-- >
-- > zip  _  0   1  (1) (2)  x  ...  <- fibs
-- >     \_  1  (1) (2)  x  ...      <- tail fibs
-- >         +   +   +   +
-- > 0   1  (1) (2)  x   x
-- >
-- > ...
-- >
-- > zip  _  0   1  (1) (2) (3) ...  <- fibs
-- >     \_  1  (1) (2) (3) ...      <- tail fibs
-- >         +   +   +   +
-- > 0   1  (1) (2) (3) (5) ...
fibsWithZip :: Integral a => [a]
fibsWithZip = 0 : 1 : zipWith (+) fibsWithZip (tail fibsWithZip)

{- Exercise 3 -}

-- | A data type to represent infinite lists of elements.
data Stream a = Cons a (Stream a)
  deriving (Eq)

-- streamHead :: Stream a -> a
-- streamHead (Cons h _) = h
--
-- streamTail :: Stream a -> Stream a
-- streamTail (Cons _ t) = t

instance Show a => Show (Stream a) where
  show = show . take 40 . streamToList

-- | Convert a stream to an (infinite) list.
streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

{- Exercise 4 -}

-- | Generate a @Stream@ by repeating the given element.
streamRepeat :: a -> Stream a
streamRepeat val = Cons val $ streamRepeat val

-- | Apply a transformation to each element in a @Stream@.
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons h t) = Cons (f h) $ streamMap f t

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f (f seed)

{- Exercise 5 -}

-- | Generate a @Stream@ of integral numbers.
nats :: Integral a => Stream a
nats = streamFromSeed (+ 1) 0

-- | Alternate the elements of two @Stream@s.
--
-- __Note__ that an implementation like:
--
-- > interleaveStreams (Cons l ls) (Cons r rs) =
-- >  Cons l $ Cons r $ interleaveStreams ls rs
--
-- would cause 'ruler' to hang. Let's consider the following helper function
-- for @ruler@:
--
-- > rulerFrom :: Integral a => a -> Stream a
-- > rulerFrom n = interleaveStreams (streamRepeat n) $ rulerFrom (n + 1)
--
-- Pattern matching on the second arg needs to evaluate @rulerFrom (n + 1)@.
-- This, in turn, needs @rulerFrom (n + 2)@ to be evaluated, and so on.
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons l ls) r =
  Cons l $ interleaveStreams r ls

-- | Map the stream @[1..]@ to the stream of the highest power of 2 that
-- divides each element.
--
-- > 0,1,0,2,0,1,0,3,0,...
--
-- A possible (and somewhat obvious) implementation is the following:
--
-- > -- Given a number n, find the highest power of 2 that divides n.
-- > maxDividingPowerTwo :: Integral a => a -> a
-- > maxDividingPowerTwo n = f 0 n
-- >   where
-- >     f pow2 num
-- >       | odd num   = pow2
-- >       | otherwise = f (pow2 + 1) (num `div` 2)
-- >
-- > ruler :: Integral a => Stream a
-- > ruler =
-- >   streamMap maxDividingPowerTwo $
-- >     streamFromSeed (+ 1) 1
--
-- However, you can interleave a number of streams and avoid the divisibility
-- check (see implementation).
ruler :: Integral a => Stream a
ruler = rulerFrom 0
  where
    rulerFrom n = interleaveStreams (streamRepeat n) $ rulerFrom (n + 1)

{- Exercise 6 -}

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Cons lh lt) (Cons rh rt) =
  Cons (f lh rh) (streamZipWith f lt rt)

scaleNumStream :: Num a => a -> Stream a -> Stream a
scaleNumStream s = streamMap (* s)

-- | Make @Stream Integer@ and instance of type class @Num@.
instance Num (Stream Integer) where
  -- \| Generate @Stream@ composed of @n,0,0,...@.
  fromInteger n = Cons n $ streamRepeat 0

  -- \| Negate all the coefficients.
  negate = streamMap negate

  -- \| Add two streams (element-wise add their elements).
  (+) = streamZipWith (+)

  -- \| Multiply two streams.
  (Cons a0 as) * b@(Cons b0 bs) = Cons (a0 * b0) ((scaleNumStream a0 bs) + as * b)

instance Fractional (Stream Integer) where
  a@(Cons a0 as) / b@(Cons b0 bs) =
    let q = a / b
     in Cons (a0 `div` b0) (scaleNumStream (1 `div` b0) (as - q * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

{- Exercise 7 -}

-- | A 2x2 matrix of @Num@ elements.
data Matrix a = M a a a a
  deriving (Show, Eq)

-- | Get the (0,0) element of a 'Matrix'
elem00 :: Matrix a -> a
elem00 (M a00 _ _ _) = a00

instance Num a => Num (Matrix a) where
  fromInteger n = let i = fromInteger n in M i 0 0 i
  negate (M a00 a01 a10 a11) = M (-a00) (-a01) (-a10) (-a11)
  (*) (M a00 a01 a10 a11) (M b00 b01 b10 b11) =
    M
      (a00 * b00 + a01 * b10)
      (a00 * b01 + a01 * b11)
      (a10 * b00 + a11 * b10)
      (a10 * b01 + a11 * b11)

  (+) (M a00 a01 a10 a11) (M b00 b01 b10 b11) =
    M (a00 + b00) (a01 + b01) (a10 + b10) (a11 + b11)

-- |
-- @fib4 n@ evaluates the @n@-th Fibonacci number by exponentiating generator
-- matrix
--
-- > F = [1 1]
-- >     [1 0]
--
-- This function runs in /O(log n)/.
--
-- This function is total: it will properly work for negative inputs too.
fib4 :: Integral a => a -> a
fib4 n
  | n == 0 = 0
  | n > 0 = elem00 $ (M 1 1 1 0) ^ (n - 1)
  | otherwise = (if even n then -1 else 1) * fib4 (-n)
