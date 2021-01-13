-- Problem 75 : http://projecteuler.net/problem=75 

import Data.List
import Debug.Trace
import Data.Function
trisums = sort [x | m <- takeWhile (<867) [1..], n <- takeWhile (<m) [1..], gcd m n == 1, odd (m-n), gcd (m*m+n*n) (2*m*n) == 1, let x = 2*m*m+2*m*n, x <= 1500000]

main = print $ length $ filter ((==1) . length) $ group $ sort $ concat[map (*t) [1..1500000 `div` t] | t <- trisums]
