{-# LANGUAGE FlexibleInstances #-}
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
--
-- __NOTE:__ @(JoinList m a, (+++), Empty)@ is not a monoid because operator
-- @(+++)@ is not associative.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty r = r
(+++) l Empty = l
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
    (Append _ l r) ->
      let leftSize = jlSize l
       in dropJ n l +++ dropJ (n - leftSize) r
    other -> other

-- | Take @n@ elements from a @JoinList@.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl
  -- Nothing to take.
  | n <= 0 = Empty
  -- Take more elements than there are in the list: return the list.
  | n >= jlSize jl = jl
  | otherwise = case jl of
    (Append _ l r) ->
      let leftSize = jlSize l
       in takeJ n l +++ takeJ (n - leftSize) r
    other -> other

{- Exercise 3 -}
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

{- Exercise 4 -}

-- | Apply a function to both elements of a pair.
(+>) :: (a -> b) -> (a, a) -> (b, b)
f +> (x, y) = (f x, f y)

-- | Split a list in half.
splitInHalf :: [a] -> ([a], [a])
splitInHalf list = splitAt (length list `div` 2) list

-- | Construct a @Single@ from a string, evaluating its score.
single :: String -> JoinList (Score, Size) String
single str = Single (scoreString str, 1) str

-- | Construct a @JoinList@ from a list of stings.
fromStrings :: [String] -> JoinList (Score, Size) String
fromStrings [] = Empty
fromStrings [x] = single x
fromStrings xs = uncurry (+++) . (fromStrings +>) . splitInHalf $ xs

-- | Replace a line at a given index by looking for the string and replacing it.
replaceLineRec ::
  -- | Index to replace
  Int ->
  -- | The new line
  String ->
  -- | The target buffer
  JoinList (Score, Size) String ->
  -- | The modified buffer
  JoinList (Score, Size) String
replaceLineRec lineIdx withLine buf
  | 0 < lineIdx || lineIdx >= numLines buf = buf
  | otherwise = case buf of
    (Single _ _) -> single withLine
    (Append _ l r) ->
      let lSize = numLines l
       in if lineIdx < lSize
            then replaceLine lineIdx withLine l +++ r
            else l +++ replaceLine (lineIdx - lSize) withLine r
    _ -> buf

-- | Replace a line at a given index by looking taking advantage of @takeJ@ and
-- @dropJ@.
replaceLineLin ::
  -- | Index to replace
  Int ->
  -- | The new line
  String ->
  -- | The target buffer
  JoinList (Score, Size) String ->
  -- | The modified buffer
  JoinList (Score, Size) String
replaceLineLin lineIdx withLine buf =
  takeJ lineIdx buf
    +++ single withLine
    +++ dropJ (lineIdx + 1) buf

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = fromStrings . lines
  line = indexJ
  numLines = jlSize
  value = getScore . fst . tag
  replaceLine = replaceLineLin
