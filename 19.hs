-- Problem 19 : http://projecteuler.net/problem=19
main = print $ length $ filter (==6) sundays where
	sundays = [dow 1 y z | y <- [1..12], z <- [1901..2000]]
	f x y z = x - 1
		+ (sum $ map (\m -> days m z) [1..y-1])
		+ (sum $ map (\y -> days 0 y) [1900..z-1])
	
	dow x y z = (f x y z) `mod` 7
	days y z 
		-- month == 0 : whole year
		|	y == 0 = 365 + leap
		|	y == 1 || y == 3 || y == 5 || y == 7 || y == 8 || y == 10 || y == 12 = 31
		| 	y == 2 = 28 + leap
		| 	otherwise = 30
		where leap 
			| z `mod` 4 == 0 && (z `mod` 100 /= 0 || z `mod` 400 == 0) = 1
			| otherwise = 0
