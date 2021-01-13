-- Problem 127 : http://projecteuler.net/problem=127 

import Data.Numbers.Primes
import Data.Array
import Data.List
import Control.Monad
import Debug.Trace

n = 120000

factors = array (1, n) [(x, primeFactors x) | x <- [1..n]]
nfactors = array (1, n) [(x, nub $ factors ! x) | x <- [1..n]]
pd = array (1,n) [(p, product $ nfactors ! p) | p <- [1..n]]

abc = [(a,b,c) | a <- [1..n `div` 2],
		let na = nfactors ! a,
		m <- [1 .. n `div` a],
		bd <- [bb | bb <- [1..a], na `intersect` (nfactors ! bb) == []],
		let b = a * m + bd, 
		let c = a + b, c < n,
		(nfactors ! b) `intersect` (nfactors ! c ) == [],
		(pd!a) * (pd!b) * (pd!c) < c,
		trace (show (a,b,c)) True]

third (a,b,c) = c
	
main = print $ sum $ map third abc