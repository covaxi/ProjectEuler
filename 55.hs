-- Problem 55 : http://projecteuler.net/problem=55 

reverseNum = read . reverse . show
 
palindrome x =
    sx == reverse sx
    where
    sx = show x 
 
lychrel = 
    not . any palindrome . take 50 . tail . iterate next
    where
    next x = x + reverseNum x
 
main = print $ length $ filter lychrel [1..10000]