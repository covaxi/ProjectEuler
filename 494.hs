-- Problem 494 : http://projecteuler.net/problem=494

import Data.List
import Data.Char

digitize' _ 0 = []
digitize' y x = mod x y : digitize' y (div x y)
toBase x y = map intToDigit $ reverse $ digitize' x y

summ x = sum $ map (digitToInt) x

collatz :: Int -> [Int]
collatz x 
    | x == 1 = [1]
    | otherwise = x : collatz c
    where c 
            | even x = x `div` 2
            | otherwise = x * 3 + 1