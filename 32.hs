-- Problem 32 : http://projecteuler.net/problem=32 

import Control.Monad
import Data.List
 
combs 0 xs = [([],xs)]
combs n xs = [(y:ys,rest) | y <- xs, (ys,rest) <- combs (n-1) (delete y xs)]
 
l2n :: (Integral a) => [a] -> a
l2n = foldl' (\a b -> 10*a+b) 0
 
swap (a,b) = (b,a)
 
explode :: (Integral a) => a -> [a]
explode = unfoldr (\a -> if a==0 then Nothing else Just . swap $ quotRem a 10)
 
pandigiticals =
  nub $ do (beg,end) <- combs 5 [1..9]
           n <- [1,2]
           let (a,b) = splitAt n beg
               res = l2n a * l2n b
           guard $ sort (explode res) == end
           return res
 
problem_32 = sum pandigiticals



l x 
  | x < 10 = 1
  | otherwise = 1 + l (x `div` 10)
  
 
 
a = [(a,b,c) | 
	a <- [2..98],
	let a1 = a `div` 10,
	let a2 = a `mod` 10,
	a1 /= a2, a2 /= 0,
	b <-[a+1..9876], 
	let x = l a + l b, 
	x == 5,
	let b1 = b `mod` 10,
	b1 /= 0, b1 /= a1, b1 /= a2,
	let b2 = (b `div` 10) `mod` 10,
	b2 /= 0, b2 /= a1, b2 /= a2, b2 /= b1,
	let b3 = (b `div` 100) `mod` 10,
	b3 /= 0, b3 /= a1, b3 /= a2, b3 /= b2, b3 /= b1,
	let b4 = (b `div` 1000) `mod` 10,
	b4 == 0 || (b4 /= a1 && b4 /= a2 && b4 /= b1 && b4 /= b2 && b4 /= b3),
	let c = a*b,
	l c == 4,
	let c1 = c `mod` 10,
	c1 /= 0, c1 /= a1, c1 /= a2, c1 /= b1, c1 /= b2, c1 /= b3, c1 /= b4,
	let c2 = (c `div` 10) `mod` 10,
	c2 /= 0, c2 /= a1, c2 /= a2, c2 /= b1, c2 /= b2, c2 /= b3, c2 /= b4, c2 /= c1, 
	let c3 = (c `div` 100) `mod` 10,
	c3 /= 0, c3 /= a1, c3 /= a2, c3 /= b1, c3 /= b2, c3 /= b3, c3 /= b4, c3 /= c1, c3 /= c2,
	let c4 = (c `div` 1000) `mod` 10,
	c4 /= 0, c4 /= a1, c4 /= a2, c4 /= b1, c4 /= b2, c4 /= b3, c4 /= b4, c4 /= c1, c4 /= c2, c4 /= c3] 
	
b = map (\(a,b,c) -> c) a

main = print $ sum $ nub b