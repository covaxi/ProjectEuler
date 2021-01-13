-- Problem 84 : http://projecteuler.net/problem=84 

import Data.List
import Data.Ord

n = 4


b0 = (1.0) : (replicate 39 (0.0))
dices = 0:0:(map prob $ group $ sort [x+y | x <- [1..n], y <- [1..n]]) where prob xs = (fromIntegral $ length xs) / n^2

            
step b = [ p | x <- [0..39],
            let p = pWithCC x
                             where 
                                pWithCC x
                                    | x `elem` [2,17,33] = pWithCH x * 14 / 16
                                    | x `elem` [0,10] = pWithCH x + pCC * 1 / 16
                                    | otherwise = pWithCH x
                                    where pCC = sum [ pWithCH t | t <- [2,17,33] ]
                                pWithCH x
                                    | x `elem` [7,22,36] = pWithJail x * 6 / 16
                                    | x == 15 = pWithJail x + pWithJail 7 * 2 / 16
                                    | x == 25 = pWithJail x + pWithJail 22 * 2 / 16
                                    | x == 5 = pWithJail x + pWithJail 36 * 2 / 16 + pCH * 1 / 16
                                    | x `elem` [0,10,11,24,39] = pWithJail x + pCH * 1 / 16
                                    | x == 12 = pWithJail x + (pWithJail 7 + pWithJail 36) * 1 / 16
                                    | x == 28 = pWithJail x + pWithJail 22 * 1 / 16
                                    | x `elem` [4,19,33] = pWithJail x + pWithJail (x+3) * 1 / 16
                                    | otherwise = pWithJail x
                                    where pCH = sum [ pWithJail t | t <- [7,22,36] ]
                                pToGetHere x = sum [dices!!t * b!!((40 + x - t )`mod` 40) | t <- [0..8]]
                                pToGet3Doubles = 1 / 4^3
                                pWithJail x
                                    | x == 10 = pToGet3Doubles + (1 - pToGet3Doubles) * (pToGetHere 10 + pToGetHere 30)
                                    | x == 30 = 0
                                    | otherwise = pToGetHere x * (1 - pToGet3Doubles)
          ]
            
main = print $ take 3 b
        where 
            b = reverse $ sortBy (comparing snd) $ zip [0..] $ head $ drop 1000 $ iterate step b0