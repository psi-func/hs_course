module QC where

import Data.List
import Test.QuickCheck

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lp ++ [x] ++ qsort rp
  where
    lp = filter (< x) xs
    rp = filter (>= x) xs

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head empty list"

minimum :: (Ord a) => [a] -> a
minimum [] = error "Prelude.minimum empty list"
minimum xs = foldl1 min xs

prop_minimum xs = QC.head (qsort xs) == QC.minimum xs

prop_minimum' xs = not (null xs) ==> QC.head (qsort xs) == QC.minimum xs

prop_permutation xs = permutation xs (qsort xs)
  where
    permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_ordered xs = ordered (qsort xs)
  where
    ordered [] = True
    ordered [x] = True
    ordered (x : r@(y : xs)) = x <= y && ordered r

prop_append xs ys =
  not (null xs)
    ==> not (null ys)
    ==> QC.head (qsort (xs ++ ys)) == min (QC.minimum xs) (QC.minimum ys)

prop_maximum xs =
  not (null xs) ==> last (qsort xs) == maximum xs

prop_idempodent xs = qsort (qsort xs) == qsort xs

prop_sort_model xs = sort xs == qsort xs

-- too slow to write by hand!

-- quickCheck( function [type])
