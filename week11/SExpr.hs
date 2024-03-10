{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- | @zeroOrMore p@ tries to parse one or more @p@. If that fails, returns an
-- empty-list parser.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- | @oneOrMore p@ tries to parse once with @p@ and then tries to parse zero or
-- more times with @p@.
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- | Parse zero or more consecutive spaces.
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

-- | Parse an /identifier/, i.e. an alphabetic char followed by zero or more
-- alphanumeric chars.
ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- | Parse an @Atom@. 
--
-- Please note that
--
-- > ghci> runParser parseAtom "2fda"
-- > Just (N 2,"fda")
--
-- Instead of returning @Nothing@ (as "2fda" is an invalid identifier).
parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

-- | Parse an @SExpr@ (i.e. an S-expression).
parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseTrimmedSExpr <* spaces
  where
    parseTrimmedSExpr = toAtom <|> toSExpr
    toAtom = A <$> parseAtom
    toSExpr = char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')'
