-- Problem 34 : http://projecteuler.net/problem=34 

digitize' 0 = []
digitize' x = mod x 10 : digitize' (div x 10)
digitize = reverse . digitize'

fac x = product [1..x]

curious x = x == sum ( map (fac) (digitize' x))

main = print $ sum c where
	c = [x | x <- [3 .. 2 * fac 9], curious x]