-- Problem 23 : http://projecteuler.net/problem=23 

import Data.Array

main = print $ sum $ cannot where
	divisors x = 1 : filter ((==0) . rem x) [2 .. x `div` 2]
	abundant x = x < (sum $ divisors x)
	-- abundants = [abundant x | x <- [0..n]]
	abundants = listArray (0,n) $ [abundant x | x <- [0..n]]
	n = 28124
	cannot = [p | p<-[1..n], not (any (\x -> ((abundants ! x) &&  (abundants ! (p-x)))) [1..p-1]) ]
