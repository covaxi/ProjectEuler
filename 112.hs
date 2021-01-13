-- Problem 112 : http://projecteuler.net/problem=112 

import Data.Char

digitize' _ 0 = []
digitize' y x = mod x y : digitize' y (div x y)
digitize x = digitize' 10 x

isIncreasing (x:[]) = True
isIncreasing (x:y:xs) = (x <= y) && isIncreasing (y:xs)

isDecreasing (x:[]) = True
isDecreasing (x:y:xs) = (x >= y) && isDecreasing (y:xs)

isBouncing x = and [(not.isIncreasing) x, (not.isDecreasing) x]

next (x,y) = (x + 1, if (isBouncing $ digitize $ x + 1) then y + 1 else y )

fnd = head $ dropWhile (\(x,y) -> y * 100 /= x*99) (iterate next (1,0))

