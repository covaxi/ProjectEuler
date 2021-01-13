-- Problem 142 : http://projecteuler.net/problem=142 

import Data.Set

squares = fromList [ x * x | x <- [1 .. 1000] ]

solve = [ (a + b + 2 * f ) `div` 2
        | i <- [1..1000]
        , j <- [1..i-1]
        , let b = i * i
        , let c = j * j
        , let f = b - c
        , member f squares
        , k <- if even b then [2,4..j-1] else [1,3..j-1]
        , let a = k * k
        , member (c - a) squares
        , member (a + b - c) squares
        ]

main = case solve of
         (x:_) -> print x
         _     -> putStrLn "limit too low"