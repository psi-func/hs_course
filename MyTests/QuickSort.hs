module QuickSort where

quickSort :: (Ord a, Eq a) => (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort _ [a] = [a]
quickSort pred ds@(x : xs) =
  let left = filter (pred x) xs
      right = filter (not . pred x) xs
   in quickSort pred left ++ [x] ++ quickSort pred right

quickTest :: (Ord a, Eq a) => [a] -> [a]
quickTest = quickSort (<)