-- Problem 26 : http://projecteuler.net/problem=26 

import Data.Array
import Data.List
import Data.Ord (comparing)


main = print $ maximumBy (comparing snd) $ zip nums $ map (len.crem) nums where
	nums = [1..999] -- [x+y | x <- [0,10..990], y<-[1,3,7,9]]
	rem' _ 0 = []
	rem' x y = y `mod` x : rem' x (y `mod` x * 10)
	crem x = drop x $ rem' x 1
	len [] = 0
	len (x:xs) = 1 + length (takeWhile (/=x) xs)

