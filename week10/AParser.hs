{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

{- Exercise 1 -}

-- | Apply a function to the first element of a pair.
--
-- This is effectively a @fmap@ for pairs operating on the first element
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f pa = Parser $ fmap (first f) . runParser pa

{- Exercise 2 -}

instance Applicative Parser where
  -- | Construct a parser that consumes no input and return its value as if it
  -- had been parsed.
  --
  -- Alternative implementation, enabling @TupleSections@:
  --
  -- > pure x = Parser $ Just . (x,)
  pure x = Parser (\s -> Just (x, s))

  -- Parser (a -> b) -> Parser a -> Parser b
  pab <*> pa =
    Parser $
      (>>= (\(f, x) -> fmap (first f) . runParser pa $ x))
        . runParser pab

  ---- Parser (a -> b) -> Parser a -> Parser b
  --pab <*> pa = Parser b
  --  where
  --    b str =
  --      let ab = runParser pab
  --          a = runParser pa
  --       in case ab str of
  --            Just (f, x) -> fmap (first f) (a x)
  --            Nothing -> Nothing
    

{- Exercise 3 -}

-- | Maps any parser to a parser that returns an empty tuple (effectively
-- making it a validator).
toVoid :: Parser a -> Parser ()
toVoid = (const () <$>)

-- | Parses character 'a' followed by character 'b'.
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- | The same as @abParser@ but returns an empty tuple instead of @('a', 'b')@.
abParser_ :: Parser ()
abParser_ = toVoid abParser

intPair :: Parser [Integer]
intPair = (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt

{- Exercise 4 -}
instance Alternative Parser where

  -- | Return a Parser that always fails (and consumes no input).
  empty = Parser (const Nothing)
  
  -- | @pl <|> pr@ try parsing with @pl@. If this succeeds, return its result.
  -- Otherwise, return the result of parsing with @pr@.
  pl <|> pr = Parser $ \x -> runParser pl x <|> runParser pr x

{- Exercise 5 -}
intOrUppercase :: Parser ()
intOrUppercase = toVoid posInt <|> toVoid (satisfy isUpper)
