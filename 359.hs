-- Problem 359 : http://projecteuler.net/problem=359 

-- N = 71328803586048 = 2^27*3^12
coords = [ (x,y) | p2 <- [0..27], p3 <-[0..12], let x = 2^p2*3^p3, let y=2^(27-p2)*3^(12-p3)]

tri x = x*(x+1) `div` 2
--tri = scanl1 (+) [0..]
p:: Integer -> Integer -> Integer
p x y
	| y == 1 = tri x
	| otherwise = (tri (z+dx)) + (y `div` 2) * m
	where
		z = 2 * (((x - 1) `div` 2) + (y `div` 2))
		dy = y  `mod` 2
		dx = (x + 1) `mod` 2
		m = 2*((dx + dy) `mod` 2) - 1
	
pp = [p x y |  p2 <- [0..27], p3 <-[0..12], let x = 2^p2*3^p3, let y=2^(27-p2)*3^(12-p3)]
main = print $ sum pp