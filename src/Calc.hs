{-# LANGUAGE FlexibleInstances #-}
module Calc where 

import qualified Parser as P 
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

data ExprT = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Exercise 1: eval
-------------------------------------------------------------------------------
eval :: ExprT -> Integer  -- Recursive definition of eval function 
eval (Lit x)   = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-------------------------------------------------------------------------------
-- Exercise 2: evalStr
-------------------------------------------------------------------------------
-- Using fmap rather than map because list of strings is not desired here 
-- fmap instance of Functor type class 
evalStr :: String -> Maybe Integer
evalStr = fmap eval . P.parseExp Lit Add Mul

-------------------------------------------------------------------------------
-- Exercise 3: Expr type class 
-------------------------------------------------------------------------------
class Expr a where 
    lit :: Integer -> a
    add :: a -> a  -> a
    mul :: a -> a  -> a

-- All instances of Expr type class must follow how functions are defined 
instance Expr ExprT where 
    lit n   = constrain $ Lit n 
    add x y = constrain $ Add x y
    mul x y = constrain $ Mul x y
{-
Here, GHC doesn't know what concrete type to use, so ambiguity is resolved with
constrain function below, which constrains type of argument as ExprT 
-}
constrain :: ExprT -> ExprT
constrain = id 

-------------------------------------------------------------------------------
-- Exercise 4: more instances for Integer, Bool, MinMax and Mod7
-------------------------------------------------------------------------------
instance Expr Integer where  -- works just like the original calculator 
    lit n   = n 
    add x y = x + y
    mul x y = x * y 

instance Expr Bool where 
    lit n   = n > 0  -- True if positive, false if negative 
    add x y = x || y  -- logical or 
    mul x y = x && y  -- logical and 

-- Wrapping these instances in newtype wrappers 
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where 
    lit n                     = MinMax n  -- Don't return any min or max value
    add (MinMax x) (MinMax y) = MinMax (max x y)  -- max for addition
    mul (MinMax x) (MinMax y) = MinMax (min x y)  -- min for multiplication

instance Expr Mod7 where  -- original calculator mod 7 
    lit n                 = Mod7 (n `mod` 7) 
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

-- ghci tests 
{-
import qualified Parser as P 
:{
testExp :: Calc.Expr a => Maybe a
testExp = P.parseExp lit add mul "(3 * -4) + 15"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
:}
-}

-------------------------------------------------------------------------------
-- Exercise 6: HasVars type class (Choice between ex 5 and 6)
-------------------------------------------------------------------------------
class HasVars a where 
    var :: String -> a 

-- Same as ExprT, except with variables 
data VarExprT = LitV Integer | AddV VarExprT VarExprT | MulV VarExprT VarExprT 
              | VarT String deriving (Show, Eq)

-- Must define how var function is used 
instance HasVars VarExprT where 
    var s = VarT s 

-- Defines how lit, add and mul functions are used 
instance Expr VarExprT where
    lit n   = LitV n
    add x y = AddV x y
    mul x y = MulV x y 

-- Looks up value for the key string 
instance HasVars (M.Map String Integer -> Maybe Integer) where 
    var = M.lookup 

-- Same functions can be interpreted as expressions 
instance Expr (M.Map String Integer -> Maybe Integer) where 
    lit n   = \_ -> Just n  -- Anything goes to just n 
    
    -- here, m represents (String, Integer) key value pair, variable with value 
    add x y = \m -> case x m of 
                    Nothing -> Nothing 
                    Just n  -> case y m of 
                        Nothing -> Nothing
                        Just m  -> Just (n + m)
    mul x y = \m -> case x m of 
                    Nothing -> Nothing 
                    Just n  -> case y m of 
                        Nothing -> Nothing
                        Just m  -> Just (n * m)


withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs