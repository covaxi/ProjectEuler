import Data.Function
main = print $ f $ 8 where
		f x
			| x == 0	= (0,0)
			| x == 1	= (0,1)
			| otherwise	= r (2 * y - x) y where y = msb (x - 1)
		-- most significant bit
		msb x
			| x == 0    = 0
			| x == 1    = 1
			| otherwise = 2 * msb ( x `div` 2 )
		
		r = sub_rotate_add `on` f
		
		-- y + rotate (y-x)
		sub_rotate_add x y = add y $ rotate $ sub y x 
		sub (a,b) (c,d) = (c - a, d - b)
		add (a,b) (c,d) = (a + c, b + d)
		-- rotate 90 counter-clockwise
		rotate (x,y) = (-y,x)
		
