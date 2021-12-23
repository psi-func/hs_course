module MergeSort where

merge :: (Ord a) => [a] -> [a] -> [a]
merge x y = merc x y []
  where
    merc [] [] acc = acc
    merc [] ds acc = acc ++ ds
    merc ds [] acc = acc ++ ds
    merc xs@(a : as) ys@(b : bs) acc = case compare a b of
      LT -> merc as ys (acc ++ [a])
      EQ -> merc as bs (acc ++ [a, b])
      GT -> merc xs bs (acc ++ [b])

mergeSort :: (Ord a) => [a] -> [a]
mergeSort x
  | length x <= 1 = x
  | otherwise =
    let (a, b) = splitAt (div (length x) 2) x
     in merge (mergeSort a) (mergeSort b)