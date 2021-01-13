-- Problem 115 : http://projecteuler.net/problem=115 
import Data.List


f x
    | x < 50 = 1
    | otherwise = (fx !! (x-1)) + 1 + (sum (take (x-50) fx))

fx = map f [0..]

fy = zip [0..] fx

fz = head $ dropWhile (\(x,y) -> y < 1000000) fy