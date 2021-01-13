-- Problem 65 : http://projecteuler.net/problem=65 

import Data.List
import Data.Ratio
import Char

e = 2 : f 2 where f x = 1 : x : 1 : f (x+2)

main = print $ sum $ map (digitToInt) $ show $ numerator $ foldr1 (\x y -> x + 1 / y) (map (%1) (take 100 e))

