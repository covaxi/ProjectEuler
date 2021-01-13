-- Problem 70 : http://projecteuler.net/problem=70 

import Data.Numbers.Primes
import Data.Ratio
import Data.List
import Data.Function
import Data.Ord


phi x = x * product[p - 1| p <- factors] `div` (product factors) where factors = nub $ primeFactors x
f x = (sort $ show x) == (sort $ show $ phi x)
main = print $ minimumBy (compare `on` (\x-> x % phi x)) [x*y | x <- primes', y <- primes', y < x, x*y <= 10000000, f (x*y)] where primes' = takeWhile (<4000) primes
 