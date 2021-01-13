-- by google
-- main = print "The prime factorization of 600,851,475,143 is 71 * 839 * 1471 * 6857"
import Data.Numbers.Primes
main =  print . maximum . primeFactors $ 600851475143


