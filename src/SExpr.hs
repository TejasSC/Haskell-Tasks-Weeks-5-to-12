{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Data.Char
import Control.Applicative

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
-- First's failure leads to Just ([],s)
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> pure []

-- mapping cons (list) function over to parsers in Applicative context
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- checks if 1st character is letter, any alphanumeric chars are ok after
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

-- Parser of atoms, either integers or alphanumeric identifiers
parseAtom :: Parser Atom
parseAtom = spaces *> ((N <$> posInt) <|> (I <$> ident))

-- parser of S-expressions, either atoms or bracketed combinations
parseSExpr :: Parser SExpr
parseSExpr = spaces *> sx <* spaces
  where
    sx =
      (A <$> parseAtom) <|>
      (char '(' *>
      (Comb <$> (zeroOrMore parseSExpr)) <*
      char ')')
