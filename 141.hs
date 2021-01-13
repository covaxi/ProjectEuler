-- Problem 141 : http://projecteuler.net/problem=141 

import Data.List

sqrtInt :: Integer -> Integer
sqrtInt = floor . sqrt . fromIntegral

isSquare :: Integer -> Bool
isSquare x = (sqrtInt x) ^ 2 == x

isSquarefree :: Integer -> Bool
isSquarefree x = all ((/= 0) . (mod x) . (^ 2)) [2 .. sqrtInt x]

squarefree :: [Integer]
squarefree = filter isSquarefree [1 ..]

task141 :: Integer -> Integer
task141 lim = sum . nub $ do
    let small = (< lim)
    a <- takeWhile (small . (^ 6)) squarefree
    b <- takeWhile (small . (* (a ^ 6)) . (^ 4)) [1 ..]
    filter isSquare . takeWhile small $ map (\t -> a ^ 3 * b * (t ^ 3 + b)) [a * b + 1 ..]

main = print $ task141 (10 ^ 12)