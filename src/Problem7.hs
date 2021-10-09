module Problem7 where

import Relude

isPrime :: Int -> Bool
isPrime 2 = True
isPrime 3 = True
isPrime i | i < 2 = False
isPrime i = [2 .. i `div` 2] & (not . any (\x -> i `mod` x == 0))

primes :: [Int]
primes = 2:3:filter filterCandidate [5..] -- [x | x <- [2..], isPrime x]
  where 
    -- go [] = []
    -- go (p:primeCandidates) = p : go (filter filterCandidate primeCandidates)

    filterCandidate x = -- (x < (prime * prime)) || (x `mod` prime /= 0)
      primes & takeWhile (\p -> p * p <= x) & all (\p -> x `mod` p /= 0)
  

ans :: Maybe Int
ans = drop 10_000 primes & viaNonEmpty head 
