main = print $ s a where
	s a = if q == 0 then r else r + s q where
		q = a `div` 10
		r = a `mod` 10
	a = 2 ^ 1000;
