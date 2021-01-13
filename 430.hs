-- Problem 430: http://projecteuler.net/problem=430
import Data.Array

e x y 
	| y == 0 	= array (0,x) (zip [0..x] ((take x $ repeat 0) ++ [x]))
	