-- Problem 124 : http://projecteuler.net/problem=124 

import Data.List
import Data.Numbers.Primes
import Data.Function
import Data.Array

n = 100000
factors = array (1, n) [(x, primeFactors x) | x <- [1..n]]
nfactors = array (1, n) [(x, nub $ factors ! x) | x <- [1..n]]
pd = array (1,n) [(p, product $ nfactors ! p) | p <- [1..n]]

rads = sortBy (compare `on` snd) [(x, pd ! x) | x <- [1..n]]
main = print $ rads !! 9999