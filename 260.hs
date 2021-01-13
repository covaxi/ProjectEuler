-- Problem 260 : http://projecteuler.net/problem=260 
import Data.List

g x y z 
    | x == y && y == z = 0
    | x == 0 && y == z = 0
    | otherwise = x + y + z

