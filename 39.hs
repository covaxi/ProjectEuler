-- Problem 39 : http://projecteuler.net/problem=39 
import Data.List
import Data.Ord (comparing)

main = print $ maximumBy (comparing snd) (zip [1..1000] (map fnd [1..1000])) where
    fnd x = length $ filter (==x) al
    al = [ l | 
		n <- [1..50], 
		m <- [n+1..50], 
		even m || even n, 
		gcd n m == 1,
		k <-[1..100], 
		let x = k*(m*m-n*n), 
		x < 1000, 
		let y = 2*m*n*k, 
		y < 1000, 
		let l = x+y+k*(m*m+n*n), 
		l < 1000]

problem_39 = head $ perims !! indexMax
    where  perims = group
                    $ sort [n*p | p <- pTriples, n <- [1..1000 `div` p]]
           counts = map length perims
           Just indexMax = elemIndex (maximum counts) $ counts
           pTriples = [p |
                       n <- [1..floor (sqrt 1000)],
                       m <- [n+1..floor (sqrt 1000)],
                       even n || even m,
                       gcd n m == 1,
                       let a = m^2 - n^2,
                       let b = 2*m*n,
                       let c = m^2 + n^2,
                       let p = a + b + c,
                       p < 1000]