-- Problem 51 : http://projecteuler.net/problem=51 
import Data.Function
import Data.List
import Data.Numbers.Primes (primes, isPrime)

fld = foldl1 ( \x y -> x*10 + y)

digitize' _ 0 = []
digitize' y x = mod x y : digitize' y (div x y)
dig x = reverse $ digitize' 10 x



primes' = [ x | 
	x <- primes,
	x > 10,
	let digged = digitize' 10 (x `div` 10),
	let sorted = sortBy (compare `on` length) $ group $ sort digged,
	let digits = filter (\x -> (length x > 2) && (head x < 3)) sorted,
	not $ null digits,
	d <- map head digits,
	t <- nub $ map (sum . take 3) $ permutations $ map (\x->10^x*10) $ elemIndices d digged,
	let tt = filter isPrime $ take (10-d) $ iterate (+t) x,
	length tt > 7
	]

main = print $ head primes'




