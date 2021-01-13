-- Problem 21 : http://projecteuler.net/problem=21 
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]
main = print $ sum $ filter (f) [2..10000] where
	f x = y /= x && z == x where
		y = sum $ divisors x
		z = sum $ divisors y
