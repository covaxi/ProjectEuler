import Data.List
import Data.Digits
import Data.Numbers.Primes

partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]

combine = map (map (unDigits 10)) . (partitions =<<) . permutations

test [] = True
test (x:xs) = isPrime x && test xs

main = print $ length $ nub $ sort $ map sort $ filter test $ combine [1..9]