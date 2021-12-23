module Return1 where

import Data.Char (toUpper)

isGreen :: IO Bool
isGreen = do
  putStrLn "Is green your favourite color?"
  inpStr <- getLine
  return ((toUpper . head $ inpStr) == 'Y')