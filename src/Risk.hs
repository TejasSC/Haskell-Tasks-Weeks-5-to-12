{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

import qualified Data.Bifunctor as BF

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = BF.first f (a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

-----------------------------------------------------------------
-- Exercise 2: battle
-----------------------------------------------------------------
-- sorts the dice rolls of a team into descending order
diceSort :: Int -> Rand StdGen [Int]
diceSort n = (map unDV) . sortBy (flip compare) <$> (replicateM n die)

-- For foldr, if attackers win then one defenders unit dies and vice versa
battleStats :: Bool -> Battlefield -> Battlefield
battleStats win (Battlefield att def) =
  if win
  then Battlefield att (def - 1)
  else Battlefield (att - 1) def

-- main battle function
battle :: Battlefield -> Rand StdGen Battlefield
battle batf = do
    -- Generates dice roll results for each of attackers and defenders
    att <- diceSort attackers
    def <- diceSort defenders
    -- Outcomes is a [Bool], where true = atts win, false = defs win
    let outcomes = (zipWith (>) att def)

    -- foldr takes base value as batf, folds with battleStats function and
    -- boolean list of outcomes
    return $ foldr battleStats batf outcomes
  where
    -- Constructs battlefield, uses 3 attacker units and 2 units if it can
    Battlefield a d = batf
    attackers = min 3 (a - 1)
    defenders = min 2 d

-----------------------------------------------------------------
-- Exercise 3: invade
-----------------------------------------------------------------
invade :: Battlefield -> Rand StdGen Battlefield
invade batf @(Battlefield att def)
  | (def == 0 || att < 2) = return batf
  | otherwise             = battle batf >>= invade  -- returns invade batf

-----------------------------------------------------------------
-- Exercise 4: successProb
-----------------------------------------------------------------
-- Checks if a match was won by attackers, there should be 0 defenders
victory :: Battlefield -> Bool
victory = (== 0) . defenders

-- Invades 1000 times, filters all wins (list <= 1000 in length), calculates
-- probability
successProb :: Battlefield -> Rand StdGen Double
successProb = liftM (\xs -> fromIntegral (length xs) / 1000.0)
            . liftM (filter victory)
            . replicateM 1000
            . invade

-- Tests the probability
main :: IO Double
main = evalRandIO $ successProb (Battlefield 5 3)
