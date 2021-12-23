module Lending where

    import Data.List (sortBy)
    import System.Directory.Internal.Prelude (Foldable, Fractional)
    
    data Tree a = Node a (Tree a) (Tree a)
                    | Empty
                    deriving (Show)

    data Direction = LeftDir
                    | RightDir
                    | Straight
                    deriving (Show)

    type Pair a = (a, a)

    lend2 amount balance = if amount < reserve * 0.5
                           then Just newBalance
                           else Nothing
        where reserve = 100
              newBalance = balance - amount

    pluralise :: String -> [Int] -> [String]
    pluralise word = map plural
        where plural 0 = "no " ++ word ++ "s"
              plural 1 = "one " ++ word
              plural n = show n ++ " " ++ word ++ "s" 

    itemName = "Weighted Companion Cube"

    fromMaybe :: p -> Maybe p -> p
    fromMaybe defval wrapped = 
        case wrapped of
            Nothing  -> defval
            Just value -> value

    absolute :: (Ord p, Num p) => p -> p
    absolute x = 
        if x >= 0
        then x
        else -x


    -- guards
    nodesAreSame :: Eq a => Tree a -> Tree a -> Maybe a
    nodesAreSame (Node a _ _) (Node b _ _)
        | a == b = Just a
    nodesAreSame _ _ = Nothing 

    lend3 :: (Ord a, Fractional a) => a -> a -> Maybe a
    lend3 amount balance
        | amount <= 0 = Nothing 
        |amount > reserve * 0.5 = Nothing 
        | otherwise  = Just newBalance
        where reserve = 100
              newBalance = balance - amount

    niceDrop :: (Ord t, Num t) => t -> [a] -> [a]
    niceDrop n xs | n <= 0 = xs
    niceDrop _ [] = []
    niceDrop n (_:xs) = niceDrop (n-1) xs

    myLength :: Num p => [a] -> p
    myLength [] = 0
    myLength (_:xs) = 1 + myLength xs

    sumList :: Num p => [p] -> p
    sumList = foldr (+) 0

    averageList xs = sumList xs / fromIntegral (myLength xs)

    palindrome xs = xs ++ helper xs []
        where helper [] acc = acc 
              helper (x:xs) acc = helper xs (x:acc)

    isPalindrome :: Eq a => [a] -> Bool
    isPalindrome xs = reverse xs == xs

    mySort :: (Foldable t) => [t a] -> [t a]
    mySort = sortBy (\a b -> compare (length a) (length b))

    intersperse :: a -> [[a]] -> [a]
    intersperse _ [] = []
    intersperse s (x:xs) = helper s xs x 
        where helper s (x:xs) acc = helper s xs (acc ++ (s:x))
              helper _ [] acc = acc


    heightTree :: (Num b, Ord b) => Tree a -> b
    heightTree Empty = 0
    heightTree (Node _ a b) = 1 + max (heightTree a) (heightTree b)

    

    pointDir :: (Num a, Ord a) => Pair a -> Pair a -> Pair a -> Direction
    pointDir (a_x, a_y) (b_x, b_y) (c_x, c_y) = 
        let (x1, y1) = (b_x - a_x, b_y - a_y)
            (x2, y2) = (c_x - b_x, c_y - b_y)
            vek = x1*y2 - x2*y1
        in case compare vek 0 of
            LT -> RightDir
            GT -> LeftDir
            EQ -> Straight

    pointDirs :: (Num a, Ord a) => [Pair a] -> [Direction]
    pointDirs [] = []
    pointDirs [_] = []
    pointDirs [_, _] = []
    pointDirs (a:b:c:xs) = pointDir a b c : pointDirs (b:c:xs)

    myDropWhile :: (a -> Bool) -> [a] -> [a]
    myDropWhile _ [] = []
    myDropWhile fun (x:xs)
        | fun x = x:xs
        | otherwise = myDropWhile fun xs


    findDropMin :: (a -> a -> Bool) -> [a] -> (a, [a])
    findDropMin _ [] = []
    findDropMin cmp (x:xs) = helper cmp xs [] x where
        helper _ [] rem min = (min, rem) 
        helper cmp (d:ds) rem min
            | cmp d min = helper cmp ds (min:rem) d
            | otherwise = helper cmp ds (d:rem) min

    makeVectorField :: Pair a -> [Pair a] -> [Pair a]
    makeVectorField _ [] = []
    makeVectorField z ds = helper z ds [] where
        helper _ [] acc = acc
        helper (x1, y1) ((x2,y2):xs) acc = helper (x1, y1) xs ((x2 - x1, y2 - y1):acc)

    
    

            

    
