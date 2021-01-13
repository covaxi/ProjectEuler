-- Problem 85 : http://projecteuler.net/problem=85 

import Data.List
import Data.Ord

f x y = y*(y+1)*x*(x+1) `div` 4
n = 2000

main = print $ minimumBy (comparing snd) [ (x*y, z) | x <- [1..2000], y <- [1..x], let z = abs $ f x y - 2000000]
    