module Problem9 where

import Relude

triples :: [[Integer]]
triples = 
  [ [a, b, c]
    | a <- [1 .. 250]
    , b <- [250 .. 500]
    , c <- [250 .. 500]
    , a*a + b*b == c*c
  ]

specialTriple :: Maybe [Integer]
specialTriple = triples & find ((== 1000) . sum)

ans :: Maybe Integer
ans = product <$> specialTriple 

