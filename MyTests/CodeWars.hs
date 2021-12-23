module CodeWars where

import Data.Bits
import Data.Char (toUpper)
import Data.List

generateHashtag :: String -> Maybe String
generateHashtag str
  | null list || l > 140 = Nothing
  | otherwise = Just hash
  where
    list = words str
    hash = (:) '#' $ concatMap (\(f : w) -> toUpper f : w) list
    l = length hash

type Ingredient = String

type Amount = Int

type Recipe = [(Ingredient, Amount)]

type Storage = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage =
  let rec' = sortBy (\(a, _) (b, _) -> compare a b) recipe
      store' = sortBy (\(a, _) (b, _) -> compare a b) storage
   in cakes' rec' store' []
  where
    cakes' [] _ acc = if null acc then 0 else minimum acc
    cakes' _ [] _ = 0
    cakes' rec@((n1, a1) : is) ((n2, a2) : ss) acc =
      if n1 == n2
        then cakes' is ss (div a2 a1 : acc)
        else cakes' rec ss acc

findOdd :: [Int] -> Int
findOdd [] = error "no valid list"
findOdd (x : xs) = foldl' xor x xs

romanNumber :: Integer -> String
romanNumber = undefined

encodeSym :: Integer -> Char
encodeSym = undefined
