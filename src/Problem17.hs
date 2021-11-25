module Problem17 where

import Relude
import Data.Char (isSpace, isLetter)


speak :: Int -> String
speak i 
  | i >= 1000 = 
    speak (i `div` 1000) ++ " thousand" 
      ++ memptyIfFalse (i `mod` 1000 > 0) (", " ++ speak (i `mod` 1000))
  | i >= 100 = 
    speak (i `div` 100) ++ " hundred" 
      ++ memptyIfFalse (i `mod` 100 > 0) (" and " ++ speak (i `mod` 100))
  | i >= 90 = "ninety" ++ memptyIfFalse (i `mod` 90 > 0) ("-" ++ speak (i `mod` 90))
  | i >= 80 = "eighty" ++ memptyIfFalse (i `mod` 80 > 0) ("-" ++ speak (i `mod` 80))
  | i >= 70 = "seventy" ++ memptyIfFalse (i `mod` 70 > 0) ("-" ++ speak (i `mod` 70))
  | i >= 60 = "sixty" ++ memptyIfFalse (i `mod` 60 > 0) ("-" ++ speak (i `mod` 60))
  | i >= 50 = "fifty" ++ memptyIfFalse (i `mod` 50 > 0) ("-" ++ speak (i `mod` 50))
  | i >= 40 = "forty" ++ memptyIfFalse (i `mod` 40 > 0) ("-" ++ speak (i `mod` 40))
  | i >= 30 = "thirty" ++ memptyIfFalse (i `mod` 30 > 0) ("-" ++ speak (i `mod` 30))
  | i >= 20 = "twenty" ++ memptyIfFalse (i `mod` 20 > 0) ("-" ++ speak (i `mod` 20))
  | i == 18 = "eighteen"
  | i == 15 = "fifteen"
  | i >= 14 = speak (i - 10) ++ "teen"
  | i == 13 = "thirteen"
  | i == 12 = "twelve"
  | i == 11 = "eleven"
  | i == 10 = "ten"
  | i == 9 = "nine"
  | i == 8 = "eight"
  | i == 7 = "seven"
  | i == 6 = "six"
  | i == 5 = "five"
  | i == 4 = "four"
  | i == 3 = "three"
  | i == 2 = "two"
  | i == 1 = "one"
  | otherwise = ""

ans = sum $ length . filter isLetter . speak <$> [1 .. 1000] 
