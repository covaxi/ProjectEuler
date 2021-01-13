-- Problem 106 : http://projecteuler.net/problem=106 

n = 7

lst = take n $ iterate (*2) 1

pairs = [[x,y] | x <- lst, y <- lst, y > x]

sumPairs = map sum pairs

nm [] = 0
nm [x] = 0
nm (x:xs) = (nm xs) + (length $ filter (<x) xs)

-- map (map sum) $ groupBy ((==) `on` length) $ sortBy (compare `on` length) $ sort $ filter ((<= 2) . length) $ subsequences $ take 5 $ iterate (*2) 1


binomial x y =(prodxy (y+1) x) `div` (prodxy 1 (x-y))
prodxy x y=product[x..y]
-- http://mathworld.wolfram.com/DyckPath.html
catalan n=(`div` (n+1)) $binomial (2*n) n
calc n=
    [e*(c-d)|
    a<-[1..di2],
    let mu2=a*2,
    let c=(`div` 2) $ binomial mu2 a,
    let d=catalan a,
    let e=binomial n mu2]
    where
    di2=n `div` 2
problem_106 = calc 12

-- map nm $ map (map sum) $ groupBy ((==) `on` length) $ sortBy (compare `on` length) $ sort $ filter ((<= (n `div` 2)) . length) $ subsequences $ take n $ iterate (*2) 1