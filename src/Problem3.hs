module Problem3 where

import Relude
import Relude.Extra (Foldable1(maximum1))

isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime 3 = True
isPrime i = [2 .. i `div` 2] & (not . any (\x -> i `mod` x == 0))

primes :: [Integer]
primes = [x | x <- [2..], isPrime x]

primeFactors :: Integer -> [Integer]
primeFactors i = case firstPrimeFactor of 
  Just pf -> pf : primeFactors (i `div` pf) 
  Nothing -> [] 

  where 
  firstPrimeFactor = find (\pf -> i `mod` pf == 0) primesToConsider 
  primesToConsider = primes & takeWhile (<= i)

ans :: [Integer]
ans = --viaNonEmpty maximum1 $ 
  primeFactors 600851475143
