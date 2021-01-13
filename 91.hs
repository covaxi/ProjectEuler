-- Problem 91 : http://projecteuler.net/problem=91 

main = print $ (length[1 |
                        x1 <- [0..50], 
                        y1 <- [0..50], 
                        x1 /= 0 || y1 /= 0, 
                        x2 <- [0..50], 
                        y2 <- [0..50], 
                        x2 /= 0 || y2 /= 0, 
                        x1 /= x2 || y1 /= y2,
                        x1*(x2-x1)+y1*(y2-y1) == 0 || (x2-x1)*x2 + (y2-y1)*y2 == 0 || x1*x2 + y1*y2 == 0]) 
                `div` 2