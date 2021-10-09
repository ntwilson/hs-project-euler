module Problem5 where

import Relude
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime 3 = True
isPrime i = [2 .. i `div` 2] & (not . any (\x -> i `mod` x == 0))

primes :: [Int]
primes = [x | x <- [2..], isPrime x]

primeFactors :: Int -> [Int]
primeFactors i = case firstPrimeFactor of 
  Just pf -> pf : primeFactors (i `div` pf) 
  Nothing -> [] 

  where 
  firstPrimeFactor = find (\pf -> i `mod` pf == 0) primesToConsider 
  primesToConsider = primes & takeWhile (<= i)

counts :: Ord a => [a] -> Map a Int
counts xs = Map.fromList [(i, countOf i) | i <- toList $ Set.fromList xs]
  where 
    countOf i = filter (== i) xs & length

merge :: Map Int Int -> Map Int Int -> Map Int Int
merge = Map.unionWith max
  
countsOfEachPrimeFactor :: Map Int Int
countsOfEachPrimeFactor = foldl' merge Map.empty (counts <$> (primeFactors <$> [1 .. 20]))

ans :: Int
ans = Map.foldlWithKey (\acc el count -> acc * (el ^ count)) 1 countsOfEachPrimeFactor 
