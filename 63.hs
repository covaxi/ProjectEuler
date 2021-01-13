-- Problem 63 : http://projecteuler.net/problem=63 

main = print $ length [1 |
    x <- [1..9],
    y <- [1..50],
    y == (length$show$x^y)]