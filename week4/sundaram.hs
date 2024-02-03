{-# OPTIONS_GHC -Wall #-}

import qualified Data.List.NonEmpty as NE (head, length, group)
import Data.List

sieveLimit :: Int
sieveLimit = 1000

main :: IO()
main =  print $ sieveSundaram sieveLimit

{- |
@sieveSundaram n@ returns all the odd prime numbers up to @2n+1@.
It uses the Sundaram sieve algorithm.
-}
sieveSundaram :: Integral a => a -> [a]
sieveSundaram n = 
    map ((+1) . (*2) . NE.head)
    . filter ((==1)  . NE.length)
    . NE.group
    . sort
    $ [1..n] ++ sundaramSieveList n

{- |
Generate the list of sieves for the Sundaram algorithm up to @n@.

Use list comprehension to :
 1. Generate all the pairs i,j such that j=1..n, i=1..j
 2. Evaluate a sieve from i,j

Then, take only the sieves that are no greater than n.
-}
sundaramSieveList :: Integral a => a -> [a]
sundaramSieveList n = filter (<=n) [ sieve i j | j <- [1..n], i <- [1..j] ]
    where sieve i j = i + j + 2 * i * j
