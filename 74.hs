-- Problem 74 : http://projecteuler.net/problem=74 

import Data.Array
import Data.Char
import Data.List
import Debug.Trace


facs = array (0,9) [(x,product[1..x]) | x <- [0..9]]
-- f x = sum $ map (\x -> facs ! digitToInt x) $ show x
f :: (Show a, Num a) => a -> Int
f = sum . map (($) (!) facs . digitToInt) . show
g = iterate f

n = 10^6

periodLength x 
    | x `mod` 1000 == 0 = trace (show (x `div` 1000) ++ "%%") len $ g x 
    | otherwise = len $ g x where 
        len (x:y:z:t:xs)
            | x == y = 1
            | x == z = 2
            | x == t = 3
            | otherwise = 1 + len (y:z:t:xs)

main = print $ length $ findIndices (==60) $ map periodLength [1..10^6]