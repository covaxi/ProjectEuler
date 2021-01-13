-- Problem 56 : http://projecteuler.net/problem=56 

import Char
s x = sum $ map (digitToInt) $ show x
main = print $ maximum [s $ x^y | x <- [1..99], y <- [1..99]]