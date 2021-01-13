-- Problem 104 : http://projecteuler.net/problem=104 

import Data.List

last10 x = (sort $ nub $ show $ x `mod` (10^10)) == "123456789"
first10 x = (sort $ nub $ take 10 $ show x) == "123456789"


f 0 _ _ = f 1 1 0
f n a b 
    | last10 a && first10 a = n
    | otherwise = f (n+1) (a+b) a

main = print $ f 0 0 0