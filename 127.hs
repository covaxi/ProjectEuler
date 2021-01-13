-- Problem 127 : http://projecteuler.net/problem=127 

import Data.Numbers.Primes
import Data.Array
import Data.List
import Control.Monad
import Debug.Trace

n = 120000

primesToUse = take 4 primes
x = product primesToUse

factors = array (1, n) [(x, primeFactors x) | x <- [1..n]]
nfactors = array (0, n) ((0, primesToUse) : [(x, nub $ factors ! x) | x <- [1..n]])
pd = array (1,n) [(p, product $ nfactors ! p) | p <- [1..n]]

a_divisors = [0..x - 1]
b_divisors = array (0, x - 1) [(p, z) | p <- a_divisors, let z = [q | q <- a_divisors, 
															(nfactors ! q) `intersect` (nfactors ! p) `intersect` primesToUse == []]]
generate (mi, ma, y) = 
	dropWhile (<= mi) $
	takeWhile (<= ma) $ [mm*x+md | mm <- [mi `div` x .. ma `div` x + 1], md <- y]

abc = [(a,b,c) | am <- [0.. n `div` x], ad <- a_divisors, 
		let a = am * x + ad, 
		a > 0,
		a < n `div` 2,
		trace (show a)  True,
		let na = nfactors ! a, 
		b <- generate (a, n-a-1, b_divisors ! ad), 
		let nb = nfactors ! b, 
		let pab = (pd ! a) * (pd ! b),
		pab < a + b,
		na `intersect` nb == [],
		let c = a + b, 
		pab * (pd ! c) < c,
		let nc = nfactors ! c,
		na `intersect` nc == [], nb `intersect` nc == [], trace (show (a,b,c)) True]

third (a,b,c) = c
	
main = print $ sum $ map third abc