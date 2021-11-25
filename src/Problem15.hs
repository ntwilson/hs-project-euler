module Problem15 where

import Relude
import Problem8 (window)

thatSpecialTriangle :: [[Integer]]
thatSpecialTriangle = iterate go [1]
  where
    go xs = 1 : (sum <$> window 2 xs) ++ [1]

