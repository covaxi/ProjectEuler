-- Problem 52 : http://projecteuler.net/problem=52 

import Data.List

digitize' _ 0 = []
digitize' y x = mod x y : digitize' y (div x y)
dig x = reverse $ digitize' 10 x

main = print $ head
	[x | 
	x <- [123456..987654],
	length (nub $ map sort $ map dig $ take 6 $ iterate (+x) x ) == 1]