module Problem14 where

import Relude
import Relude.Extra (maximum1)
import Data.Function.Memoize (Memoizable(memoize))

collatz :: Int -> Int
collatz i 
  | even i = i `div` 2
  | otherwise = 3*i + 1

collatzChain :: Int -> [Int]
collatzChain = memoize go
  where
    go 1 = [1]
    go x = x : collatzChain (collatz x)

collatzLength :: Int -> Int
collatzLength = memoize go
  where
    go 1 = 1
    go x = 1 + collatzLength (collatz x)

ans :: Maybe (Int, Int)
ans = 
  zip ([1 .. 999_999] <&> collatzLength) [1..]
    & viaNonEmpty maximum1
