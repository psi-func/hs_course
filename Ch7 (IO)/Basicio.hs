module BasicIO where

main = do
  putStrLn "Greetengs! What is your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
