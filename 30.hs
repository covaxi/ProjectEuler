-- Problem 30 : http://projecteuler.net/problem=30 

main = print $ sum [z | a <- [0..3], b <- [0..9], c <- [0..9], d <- [0..9], e <- [0..9], f <- [0..9], 
	let z = a*100000+b*10000+c*1000+d*100+e*10+f, a^5+b^5+c^5+d^5+e^5+f^5 == z, z > 1]
             
