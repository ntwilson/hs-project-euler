module Problem12 where

import Relude
import qualified Relude.Unsafe as Unsafe

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

-- taken from https://wiki.haskell.org/Generic_number_type#squareRoot
squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
  let 
    twopows = iterate (^!2) 2
    (lowerRoot, lowerN) =
      Unsafe.last $ takeWhile ((n >=) . snd) $ zip (1:twopows) twopows
    newtonStep x = div (x + div n x) 2
    iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
    isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in Unsafe.head $ dropWhile (not . isRoot) iters


factors :: Int -> [Int]
factors i = do
  lower <- [1 .. squareRoot i]
  let (upper, remainder) = divMod i lower
  guard $ remainder == 0
  [lower, upper]

triangleNumbers :: [Int]
triangleNumbers = triangle <$> [1..]
  where
    triangle n = sum [1 .. n]

firstTriangleWith500Divisors :: Maybe Int
firstTriangleWith500Divisors = 
  find ((>= 500) . length . factors) triangleNumbers