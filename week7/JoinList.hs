{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Buffer
import Scrabble
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

{- Exercise 1 -}

-- | Get the annotation at the root of a @JoinList@.
tag :: Monoid m => JoinList m a -> m
tag jl = case jl of
  Empty -> mempty
  (Single m _) -> m
  (Append m _ _) -> m

-- | Append two @JoinLists@s.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

{- Exercise 2 -}

-- | Get size of a @JoinList@ if the tag represents the list size.
jlSize :: (Monoid b, Sized b) => JoinList b a -> Int
jlSize = getSize . size . tag

-- | Safe index into a @JoinList@. Cost /O(log n)/ for a balanced @JoinList@
-- with /n/ nodes.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl
  | 0 <= i && i < jlSize jl = case jl of
      (Single _ v) -> Just v
      (Append _ l r) ->
        let ls = jlSize l
         in if i < ls
              then indexJ i l
              else indexJ (i - ls) r
      -- NOTE: This should never be hit as @jlSize Empty == 0@.
      _ -> Nothing
  | otherwise = Nothing

-- | Convert a @JoinList m a@ to a @[a]@ with an in-order traversal.
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ x) = [x]
jlToList (Append _ l r) = jlToList l ++ jlToList r

-- | Drop @n@ elements from a @JoinList@.
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl
  -- Nothing to drop.
  | n <= 0 = jl
  -- Drop more elements than there are in the list.
  | n >= jlSize jl = Empty
  | otherwise = case jl of
      (Append m l r) ->
        let ls = jlSize l
         in if n < ls
              then Append m (dropJ n l) r
              else dropJ (n - ls) r
      -- NOTE: This should never be hit.
      _ -> Empty

-- | Take @n@ elements from a @JoinList@.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl
  -- Nothing to take.
  | n <= 0 = Empty
  -- Take more elements than there are in the list.
  | n >= jlSize jl = jl
  | otherwise = case jl of
      (Append m l r) ->
        let ls = jlSize l
         in if n < ls
              then takeJ n l
              else Append m l (takeJ (n - ls) r)
      -- NOTE: This should never be hit.
      _ -> Empty

{- Exercise 3 -}
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

{- Exercise 4 -}

(+>) :: (a -> b) -> (a, a) -> (b, b)
f +> (x, y) = (f x, f y)

fromStrings :: [String] -> (JoinList (Score, Size) String)
fromStrings [] = Empty
fromStrings [str] = Single (scoreString str, 1) str
fromStrings strings =
  let n = length strings
   in uncurry (+++)
        . (fromStrings +>)
        . splitAt (n `div` 2)
        $ strings

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = fromStrings . lines
  line = indexJ
  numLines = jlSize
  value = getScore . fst . tag
  replaceLine lineIdx withLine buf
    | 0 < lineIdx || lineIdx >= numLines buf = buf
    | otherwise = case buf of
        (Single _ _) -> Single (scoreString withLine, 1) withLine
        (Append _ l r) ->
          let lSize = numLines l
           in if lineIdx < lSize
                then replaceLine lineIdx withLine l +++ r
                else l +++ replaceLine (lineIdx - lSize) withLine r
        _ -> buf
