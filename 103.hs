-- Problem 103 : http://projecteuler.net/problem=103 

import Debug.Trace
import Data.List
import Data.Function

subsets [] = [[]]
subsets (x:xs) = (subsets xs) ++ (map (x:) $ subsets xs)

sorted lst = concat $ map (sortBy (compare `on` sum) ) $ groupBy ((==) `on` length) $ sortBy (compare `on` length) $ subsequences $ sort lst

isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = x < head xs && isSorted xs

isSortedSum lst = isSorted $ map sum $ sorted lst

lists 2 = [[x,y] | x <-[19..28], y <- [x+1..40]]
lists x = [l1 | l <- lists (x-1), y <- [last l + 1 .. minimum[271 - sum l, last l + head l - 1]], let l1 = l++[y], isSortedSum l1]
        