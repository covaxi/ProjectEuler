-- Problem 108 : http://projecteuler.net/problem=108 

import Data.Numbers.Primes
import Data.List
import Data.Ratio

numDivisors x = product $ map (+1) $ map (*2) $ map (length) $ group $ primeFactors x
numRatios x = (numDivisors (x) + 1) `div` 2

fnd = head $ dropWhile (\(a,b)->b<=1000) $ map (\x -> (x, numRatios x)) [1..]

