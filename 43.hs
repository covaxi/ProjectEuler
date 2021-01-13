-- Problem 43 : http://projecteuler.net/problem=43 

import Data.List

fld = foldl1 ( \x y -> x*10 + y)
perms = [ x | x <- permutations [0..9],
	(x !! 3) `mod` 2 == 0,
	(x !! 2 + x !! 3 + x !! 4) `mod` 3 == 0,
	(x !! 5) `mod` 5 == 0,
	(100 * (x !! 4) + 10 * (x !! 5) + x !! 6) `mod` 7 == 0,
	(100 * (x !! 5) + 10 * (x !! 6) + x !! 7) `mod` 11== 0,
	(100 * (x !! 6) + 10 * (x !! 7) + x !! 8) `mod` 13== 0,
	(100 * (x !! 7) + 10 * (x !! 8) + x !! 9) `mod` 17== 0]
	
main = print $ sum $ map fld perms