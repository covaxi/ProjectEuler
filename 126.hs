-- Problem 126 : http://projecteuler.net/problem=126 

import Data.List
import Debug.Trace

f a b c n = 2 * (a*b+a*c+b*c) + 4 * (n-1) * (a+b+c+n-2)

r = map (\x -> (head x, length x)) $ group $ sort [f a b c n | a <- [1..200], trace (show a) True, b <- [a..200], c <- [b..200], n <- [1..25]]


