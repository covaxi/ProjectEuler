-- Problem 110 : http://projecteuler.net/problem=110 

import Data.Numbers.Primes
import Data.List
import Data.Ratio

numDivisors x = product $ map (+1) $ map (*2) $ map (length) $ group $ primeFactors x
numRatios x = (numDivisors (x) + 1) `div` 2

aa = 180180*17*19*23*29*31
fnd = head $ dropWhile (\(a,b)->b<=4000000) $ map (\x -> (x, numRatios x)) [aa, 2*aa..]

