-- Problem 72 : http://projecteuler.net/problem=72 

import Data.Numbers.Primes
import Data.Ratio
import Data.List
import Data.Function
import Data.Ord

phi x = x * product[p - 1| p <- factors] `div` (product factors) where factors = nub $ primeFactors x

main = print $ sum $ map phi [2..1000000]
