main = print [(x, y, z, x*y*z) | x <- [1..1000], y <- [x..1000], let z = 1000-x-y, x*x+y*y == z*z]
