module Main where

newtype Queen = Q {getPair :: (Int, Int)}
  deriving (Show)

attack :: Queen -> Queen -> Bool
attack a b = x_a == x_b || y_a == y_b || diff_x == diff_y
  where
    (x_a, y_a) = getPair a
    (x_b, y_b) = getPair b
    diff_x = abs (x_b - x_a)
    diff_y = abs (y_b - y_a)

place :: Int -> [Queen] -> [[Queen]]
place boardSize queens
  | length queens == boardSize = [queens]
  | null candidates = []
  | otherwise = concatMap (place boardSize) traces
  where
    row = length queens
    candidates = filter (validCandidate queens) $ map (\col -> Q (row, col)) [0 .. boardSize - 1]
    traces = map (: queens) candidates

validCandidate :: [Queen] -> Queen -> Bool
validCandidate [] _ = True
validCandidate queens q = not $ any (attack q) queens

main :: IO ()
main = do
  putStrLn "What is board size?"
  inpStr <- getLine
  print $ show $ length $ place (read inpStr :: Int) []
