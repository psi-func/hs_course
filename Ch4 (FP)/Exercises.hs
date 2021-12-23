module Ch4.Exercises where

import Data.List (tails)

-- it 's as pattern
suffixes :: [a] -> [[a]]
suffixes xs@(_ : xs') = xs : suffixes xs'
suffixes _ = []

suffixes2 :: [a] -> [[a]]
suffixes2 = init . tails
