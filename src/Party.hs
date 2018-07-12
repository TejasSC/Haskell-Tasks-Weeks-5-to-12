{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where 

import Data.Tree
import Employee (Employee, GuestList)

import qualified Data.Monoid as DM
import qualified Employee as E

-------------------------------------------------------------------------------
-- Exercise 1: glCons, Monoid instance for GuestList, moreFun
-------------------------------------------------------------------------------
-- Adds an employee to GuestList 
glCons :: Employee -> GuestList -> GuestList
glCons emp@E.Emp{E.empFun = x} (E.GL emps fun) = E.GL (emp:emps) (fun + x)

-- <> must be an associative binary operation 
instance Semigroup GuestList where 
    (E.GL a funA) <> (E.GL b funB) = E.GL (a++b) (funA + funB)

-- Monoid is special case of Semigroup with identity element mempty 
instance DM.Monoid GuestList where 
    mempty  = E.GL [] 0
    mappend = (<>)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = case compare a b of 
    LT -> b
    _  -> a 

-------------------------------------------------------------------------------
-- Exercise 2: treeFold, combineGLs
-------------------------------------------------------------------------------
-- func will be replaced by nextLevel below, type mathces exactly  
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold func (Node {rootLabel = r, subForest = subTrees}) = 
    func r $ map (treeFold func) subTrees
-- r is the boss of the company, a is the type employee later on 

-------------------------------------------------------------------------------
-- Exercise 3: nextLevel 
-------------------------------------------------------------------------------
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss others = (maxM withB, maxM withoutB)  
  where 
    (withoutB, withoutSubs) = unzip others
    withB = map (glCons boss) withoutSubs 

-- maximum [] == exception, therefore maxM prevents that 
maxM :: (Monoid m, Ord m) => [m] -> m
maxM []   = mempty
maxM list = maximum list 

-------------------------------------------------------------------------------
-- Exercise 4: maxFun
-------------------------------------------------------------------------------
maxFun :: Tree Employee -> GuestList
maxFun companyTree = moreFun result1 result2 
  where 
    (result1, result2) = treeFold (nextLevel) companyTree

-------------------------------------------------------------------------------
-- Exercise 5: main
-------------------------------------------------------------------------------
-- read :: String -> Tree Employee 
-- maxFun :: Tree Employee -> GuestList
-- glToString :: GuestList -> String 
-- putStrLn :: String -> IO ()
main :: IO ()
main = readFile "company.txt" >>= putStrLn . glToString . maxFun . read 

glToString :: GuestList -> String
glToString (E.GL emps fun) = "Total fun: " ++ show fun ++ "\n" ++ 
                           unlines (map E.empName emps)