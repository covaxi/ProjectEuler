-- Problem 301 : http://projecteuler.net/problem=301 
import Data.Bits

fibs = 1:1:zipWith (+) fibs (tail fibs)

main = print $ fibs !! 31