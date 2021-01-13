-- Problem 45 : http://projecteuler.net/problem=45 

nextPen (x,y) = (x+1,y+3*x+1)
nextHex (x,y) = (x+1,y+4*x+1)

next (pen@(a,b), hex@(c,d)) 
	| b >= d = ((a,b), nextHex(c,d))
	| b < d  = (nextPen(a,b), (c,d))
	
main = print $ take 3 $ filter (\((a,b),(c,d)) -> b == d) $ iterate next ((1,1),(1,1))