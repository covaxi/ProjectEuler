-- Problem 24 : http://projecteuler.net/problem=24 
f [] ys = ys
f (x:xs) ys = z : f (xs) ([d | d <- ys, d /= z]) where z = ys !! x

fac n = product [1..n]

k 0 _ = []
k n p = n `div` (fac p) : k (n `mod` (fac p)) (p - 1)

main = print $ foldl1 (\x y -> x*10+y) (f (k (10^6 - 1) 9) [0..9])
	
	
