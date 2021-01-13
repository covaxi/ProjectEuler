-- Problem 119 : http://projecteuler.net/problem=119 
import Data.List
import Data.Digits

compare' (_,_,x,_) (_,_,y,_) = compare x y

powers = sortBy compare' $ [(x,y,z,s) | x <- [1..100], y <- [2..100], let z = x ^ y, z > 10, let s = sum $ digits 10 z, s == x]

main = print $ powers !! 29