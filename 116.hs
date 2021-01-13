-- Problem 116 : http://projecteuler.net/problem=116 

fr x
    | x < 4 = x
    | otherwise = (fr' !! (x-1)) + (fr' !! (x-2))

fr' = map fr [0..]


fb x
    | x < 6 = x - 1
    | otherwise = (fb' !! (x-1)) + (fb' !! (x-3))

fb' = map fb [0..]

fg x
    | x < 8 = x - 2
    | otherwise = (fg' !! (x-1)) + (fg' !! (x-4))

fg' = map fg [0..]


fall x = fg x + fb x + fr x - 3