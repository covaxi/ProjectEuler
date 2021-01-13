-- Problem 114 : http://projecteuler.net/problem=114 
import Debug.Trace

f x
    | x < 3 = 1
    | otherwise = (fx !! (x-1)) + 1 + (sum (take (x-3) fx))

fx = map f [0..]

