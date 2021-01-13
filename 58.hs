-- Problem 58 : http://projecteuler.net/problem=58 

import Debug.Trace
import Data.Numbers.Primes
import Data.List
import Data.Set (fromList, member)

f x     
 | even x    = error "x should be odd"
 | x == 1    = 0
 | otherwise = length $ filter (isPrime) [x*x-x+1,x*x-2*x+2,x*x-3*x+3]



main = print $ find (\(a,b) -> b*10 < a+a-1) $ zip ([3,5..]) $ scanl1 (+) $ map f [3,5..]