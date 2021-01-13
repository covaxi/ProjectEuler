-- Problem 44 : http://projecteuler.net/problem=44 
import Data.Set

pentas = [x * (3 * x - 1) `div` 2 | x <- [1..10000]]
pentas' = fromList pentas
main = print [a-b | 
	a <- pentas,
	b <- takeWhile ( <a ) pentas,
	(a - b) `member` pentas',
	(a + b) `member` pentas']