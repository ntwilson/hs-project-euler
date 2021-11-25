module Problem16 where

import Relude 

ans :: Maybe Int
ans = 
  traverse (readMaybe . singleton :: Char -> Maybe Int) (show (2 ^ 1000) :: String)
  <&> sum

  where
    singleton x = [x]
