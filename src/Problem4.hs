module Problem4 where

import Relude
import Relude.Extra (maximum1)

isPalindromic :: Show a => a -> Bool
isPalindromic i = str == reverse str 
  where str = show i 

allProductsOf2Digits :: [Integer]
allProductsOf2Digits = [x*y | x <- [10 .. 99], y <- [10 .. 99]]
allProductsOf3Digits :: [Integer]
allProductsOf3Digits = [x*y | x <- [100 .. 999], y <- [100 .. 999]]

palindromes :: [Integer]
palindromes = filter isPalindromic allProductsOf3Digits

ans :: Maybe Integer
ans = viaNonEmpty maximum1 palindromes
