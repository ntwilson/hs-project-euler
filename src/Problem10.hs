module Problem10 where

import Relude

primes :: [Int]
primes = 2:3:filter filterCandidate [5..] 
  where 
    filterCandidate x = 
      primes & takeWhile (\p -> p * p <= x) & all (\p -> x `mod` p /= 0)

ans :: Int
ans = primes & takeWhile (<= 2_000_000) & sum
