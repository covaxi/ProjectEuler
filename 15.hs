main = print s where
	s = sum l
	l = map (\x -> x*x) [c k n | k <- [0..n]]
	c k n = product[n-k+1..n] `div` product [1..k]
        n = 20
