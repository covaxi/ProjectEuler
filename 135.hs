-- Problem 135 : http://projecteuler.net/problem=135 

import Data.List

main = let n = 10^6 in print $ length $ filter (==10) $ map length $ group $ sort [x*k | x <- [1..n], k <- [1..minimum [3*x-1, div n x]], mod (x+k) 4 == 0]