module Problem2 where

import Relude
import qualified Relude.List as List

fibs :: [Int]
fibs = 1:2:go 1 2
  where
    go a b = (a+b):go b (a+b)

ans :: Int
ans = fibs & List.takeWhile (<= 4_000_000) & List.filter even & sum

