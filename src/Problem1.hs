module Problem1 where

import Relude
import qualified Relude.List as List

nats :: [Integer]
nats = [1..]

multiplesOf3Or5 :: [Integer]
multiplesOf3Or5 = nats & List.filter (\i -> i `mod` 3 == 0 || i `mod` 5 == 0)

ans :: Integer
ans = multiplesOf3Or5 & List.takeWhile (< 1000) & sum
