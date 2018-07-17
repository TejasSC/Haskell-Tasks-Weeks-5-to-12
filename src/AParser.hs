{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Control.Monad
import Data.Char

import qualified Data.Bifunctor as BF

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
-----------------------------------------------------------------
-- Exercise 1: Functor instance for Parser
-----------------------------------------------------------------
first :: (a -> b) -> (a,c) -> (b,c)
first f (arg1, arg3) = BF.first f (arg1, arg3)

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser a) = Parser $ fmap (first f) . a

-----------------------------------------------------------------
-- Exercise 2: Applicative instance for Parser
-----------------------------------------------------------------
instance Applicative Parser where
  pure a = Parser $ \s -> Just(a, s)  -- consumes no input, returns a
  p1 <*> p2 = Parser $ \s -> case runParser p1 s of
                        Nothing        -> Nothing
                        Just (f, rest) -> first f <$> runParser p2 rest

-----------------------------------------------------------------
-- Exercise 3: abParser, abParser_, intPair
-----------------------------------------------------------------
-- function in fmap is one that takes two args and puts them into a pair
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- function in fmap is one that maps any two args to ()
abParser_ :: Parser ()
abParser_ = void abParser

-- specifically maps any two integers separated by anything to [num1, num2]
intPair :: Parser [Integer]
intPair = (\num1 _ num2 -> [num1, num2]) <$> posInt <*> char ' ' <*> posInt

-----------------------------------------------------------------
-- Exercise 4: Alternative instance for Parser
-----------------------------------------------------------------
instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  p1 <|> p2 = Parser $ \s -> (runParser p1 s) <|> (runParser p2 s)
-----------------------------------------------------------------
-- Exercise 5: intOrUppercase
-----------------------------------------------------------------
-- (<|>) :: f a -> f a -> f a, f == (\_ -> ()) in this case
intOrUppercase :: Parser ()
intOrUppercase = (void posInt) <|> (void (satisfy isUpper))
