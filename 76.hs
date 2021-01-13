-- Problem 76 : http://projecteuler.net/problem=76 

import Debug.Trace

--f x y | trace (show x ++ ":" ++ show y) False = undefined
f _ 1 = 1
f x 2 = x `div` 2 + 1
f x y | y >= x = 1 + f x (x-1)
f x y = sum $ [f (x-a) a | a <- [1..y]]
main = print $ f 100 99
