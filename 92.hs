-- Problem 92 : http://projecteuler.net/problem=92 

import Data.Array
import Data.Char
import Data.List

g x = sum $ map (\x -> x*x) $ map (digitToInt) $ show x

res = listArray (1,10000000) $ 1 : [f x | x <- [2..10000000]]
        where 
            f :: Int -> Int
            f x
                | x == 89 || x == 1 = x
                | otherwise = res ! g x

main = print $ length $ filter (==89) $ elems res