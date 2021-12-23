module Ch4.Exercises1 where

import Data.Char (digitToInt)
import Data.List (foldl')
import Prelude hiding (any, concat, takeWhile)

type ErrorMessage = String

asIntFold :: String -> Int
asIntFold xs =
  if sig
    then negate num
    else num
  where
    (num, sig, _) = foldl step (0, False, True) xs
      where
        step (zero, sign, state) el
          | state && el == '-' = (zero, True, False)
          | otherwise = (zero * 10 + digitToInt el, sign, False)

asIntEither :: String -> Either ErrorMessage Int
asIntEither xs =
  if null msg
    then if sig then Right (negate num) else Right num
    else Left msg
  where
    (num, sig, _, msg) = foldl step (0, False, True, "") xs
      where
        step (zero, sign, state, fault) el
          | state && el == '-' = (zero, True, False, fault)
          | el `notElem` ['1' .. '9'] = (zero, sign, False, "non-digit '" ++ el : "'")
          | otherwise = (zero * 10 + digitToInt el, sign, False, fault)

concat :: [[a]] -> [a]
concat = foldr (++) []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile pred (x : xs)
  | pred x = x : takeWhile pred xs
  | otherwise = []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold pred = foldr (\x acc -> if pred x then x : acc else []) []

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f xs = pref : acc
  where
    (pref, acc) = foldr step ([], []) xs
      where
        step x ([], []) = ([x], [])
        step x (cur, zero)
          | f x (head cur) = (x : cur, zero)
          | otherwise = ([x], cur : zero)

any :: (a -> Bool) -> [a] -> Bool
any f = foldl' step False
  where
    step zero x
      | zero = True
      | otherwise = f x