-- Problem 254 : http://projecteuler.net/problem=254 

import Debug.Trace
import Data.List
import Data.Array
import Data.Function
import Data.Maybe

digitize' _ 0 = []
digitize' y x = mod x y : digitize' y (div x y)
digitize x = reverse $ digitize' 10 x





f x 
	| x < 10 	= product[1..x]
	| otherwise = (\(a,b) -> f a + f b) $ x `divMod` 10


sf x = sumDigits $ f x

gg x = fst $ head $ dropWhile (\(a,b)->b /= x) [(x,sf x) | x <- [1..]]

sgg x = sum $ digitize $ gg x

main1 = print $ sum $ map sg [1..20]


-- p - number of members
first p x
	| x < p		= error "Underflow"
	| a > p 	= error "Overflow"
	| otherwise = b : ( take (p-1) $ repeat 9 )
		where
			(a,b) = x `divMod` 9

allSums2 _ _ 0 = [[]]
allSums2 _ 0 x 
	| x /= 0    = error $ "i = 0 && x = " ++ show x
	| otherwise = [[]]
allSums2 m 1 x
	| m < x    	= error $ "m < x " ++ show m ++ " < " ++ show x
	| otherwise = [[x]]
allSums2 m i x = [p:xp | True
					-- trace ("m=" ++ show m ++ " i=" ++ show i ++ " x=" ++ show x ++ " p<-[" ++ show(min(x - i + 1) m) ++ ".." ++ show((x + i - 1) `div` i) ++ "]") True
					, p <- [min(x - i + 1) m, (min(x - i + 1) m) - 1 .. (x + i - 1) `div` i]
					-- , trace ("m=" ++ show m ++ " i=" ++ show i ++ " x=" ++ show x ++ " : trying p=" ++ show p ++ " xp<-allSums2 " ++ show(min p (x-p)) ++ " " ++ show(i-1) ++ " " ++ show(x-p)) True
					, xp <- allSums2 (min p (x-p)) (i-1) (x-p)
					-- ,trace ("m=" ++ show m ++ " i=" ++ show i ++ " x=" ++ show x ++ " xp=" ++ show xp) True
					]

allSums x = [xp | p <- [(x + 8) `div` 9..x], xp <- allSums2 (min x 9) p x]


s = array (1,9) [(x, product[1..x]) | x <- [1..9]]

-- ss :: Int -> [[Int]]
ss' 0 = [[]]
ss' x = sortBy (compare `on` sum) $ concat [map (p:) $ ss' (x-1) | p <- [0..x]]

ss = concat $ map ((map reverse).sortBy(\a b->compare b a).(map reverse)) $ groupBy ((==) `on` sum) $ sortBy (compare `on` sum) $ ss' 9

f' x 1 = head x
f' x y = (s ! y) * (head x) + f' (tail x ) (y-1)

sumDigits x 
	| x < 10 	= x
	| otherwise = (\(a,b) -> b + sumDigits a) $ x `divMod` 10


sf' x = sumDigits $ f' x 9

numDigitsBefore x = max 1 ((x+1) `div` 9)

g' :: Integer -> [[Integer]]
g' x = [n + head ns : tail ns | nn <- [numDigitsBefore x ..], m <- [1..3], let n =max 0 (10^nn `div` s!9 - sum[1..9]-3+m), ns <- ss]
g x = fromJust $ find (\t -> sf' t == x) $ g' x

sg x = sum $ zipWith (*) (g x) [9,8..1]

solve254 = sum [r | x <- [1..20], trace ("x=" ++ show x) True, let r = sg x, r > 0]


