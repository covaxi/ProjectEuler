-- Problem 100 : http://projecteuler.net/problem=100

import Data.Array

sqr x = (round . sqrt) $ fromIntegral x
isSquare x = (sqr x)^2 == x

tri = listArray (1, 10000000) $ scanl1 (+) [0..10000000]


f a b
    | ta > tb = f a (b+1)
    | ta < tb = f (a+1) b
    | otherwise = (a,b)
    where
        ta = a*(a-1)
        tb = 2 * b*(b-1)

756872327473

nextAB a b
    |a+b>10^12 =[a,b]
    |otherwise=nextAB (3*a+2*b+2) (4*a+3*b+3)
problem_100=(+1)$head$nextAB 14 20

aaa :: String -> String
aaa = const ([])


-- Result: (4684660,3312555) [Euler is down]
main = print $ f 1414000 1000000
