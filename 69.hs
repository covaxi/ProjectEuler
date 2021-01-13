-- Problem 69 : http://projecteuler.net/problem=69 

import Data.Numbers.Primes
import Data.Ratio
import Data.List
import Data.Function
import Data.Ord

phi x = x * product[p - 1| p <- factors] `div` (product factors) where factors = nub $ primeFactors x

main = print $ maximumBy (compare `on` (\x -> x % phi x)) [2*3*5*7*x | x <- [1..10^6 `div` (2*3*5*7)]]

