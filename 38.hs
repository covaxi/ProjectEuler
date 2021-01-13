-- Problem 38 : http://projecteuler.net/problem=38 
import Data.List

digitize' _ 0 = []
digitize' y x = mod x y : digitize' y (div x y)
dig x = reverse $ digitize' 10 x

f k x = concat $ map dig (zipWith (*) [1..k] (repeat x))
p k x = [1..9] == (sort $ f k x)

fld = foldl1 ( \x y -> x*10 + y)

main = print $ maximum [fld $ f y x | x <- [1..9999], y <- [2..9], p y x]