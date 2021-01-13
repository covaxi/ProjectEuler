-- Problem 40 : http://projecteuler.net/problem=40 

import Char
d = concat $ map (show) [0..400000]
p x = digitToInt $ d !! x

main = print $ (p 1000000) * (p 100000) * (p 10000) * (p 1000) * (p 100) * (p 10) * (p 1)
	