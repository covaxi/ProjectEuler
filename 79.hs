-- Problem 79 : http://projecteuler.net/problem=79 

import Data.List
import Data.Char

parts = map show [319,680,180,690,129,620,762,689,762,318,368,710,720,710,629,168,160,689,716,731,736,729,316,729,729,710,769,290,719,680,318,389,162,289,162,718,729,319,790,680,890,362,319,760,316,729,380,319,728,716]

g x [a,b,c]
    | x == c = [b]
    | x == b = [a]
    | otherwise = []
    
f x = nub $ concat $ map (g x) parts

g' x [a,b,c]
    | x == a = [b]
    | x == b = [c]
    | otherwise = []
    
f' :: Char -> [Char]
f' x = nub $ concat $ map (g' x) parts

ff :: Char -> [[Char]]
ff x
    | x == '/' = [[]]
    | otherwise = prd  ++ [y | a <- f' x, let y = [x,a], all (\t -> not (y `isPrefixOf` t)) prd] 
    where
        prd = ff(pred x)
    

-- [7,3,1,6,2,8,9,0]


test x = [ check y x | y <- parts ] where 
    check (x:xs) (ys)
        | x `elem` ys = check (xs) (tail $ dropWhile (/=x) ys)
        | otherwise = False
    check [] _ = True