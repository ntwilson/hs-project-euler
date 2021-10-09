module Problem6 where

import Relude

sumOfSquares :: [Int] -> Int
sumOfSquares xs = sum [x ^ 2 | x <- xs]

squareOfSums :: [Int] -> Int
squareOfSums xs = sum xs ^ 2

diffs :: [Int] -> Int
diffs xs = squareOfSums xs - sumOfSquares xs

ans :: Int
ans = diffs [1 .. 100]