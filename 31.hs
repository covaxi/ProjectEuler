-- Problem 31 : http://projecteuler.net/problem=31 

coins = [200,100,50,20,10,5,2,1]
summa = 200



m = map (summa `div`) coins

t = [z | a <- [0..m!!0], 
	b <- [0..m!!1],
	ff [a,b] <= summa,
	c <- [0..m!!2],
	ff [a,b,c] <= summa,
	d <- [0..m!!3],
	ff [a,b,c,d] <= summa,
	e <- [0..m!!4],
	ff [a,b,c,d,e] <= summa,
	f <- [0..m!!5],
	ff [a,b,c,d,e,f] <= summa,
	g <- [0..m!!6],
	ff [a,b,c,d,e,f,g] <= summa,
	let h = summa - ff [a,b,c,d,e,f,g],
	let z = a:b:c:d:e:f:g:h:[]]
	where ff x = sum ( zipWith (*) coins x )
	
main = print $ length t