-- isPrime n = n == head (primeFactors n)
isPrime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes
 
primeFactors n | n > 1 = go n primes
   where
     go n ps@(p:ps')
        | p*p > n        = [n]
        | n `rem` p == 0 =  p : go (n `quot` p) ps
        | otherwise      =      go n ps'

primes = 2 : filter isPrime [3,5..]

main = print $ sum (takeWhile (<2000000) primes)
