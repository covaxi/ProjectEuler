-- Problem 156 : http://projecteuler.net/problem=156 
import Data.Char

digits :: Int -> [Int]
digits = map (digitToInt) . show

joinLists :: [[x]] -> [x]
joinLists [] = []
joinLists (x:xs) = x ++ joinLists xs

f :: Int -> Int -> Int
f x d = (map (\x -> f_ x d) [1..] !! x)
    where 
        f_ :: Int -> Int -> Int
        f_ n d = length $ filter (==d) $ joinLists $ map digits [0..n]