module Basicio_nodo where

main =
  putStrLn "Greetings! What is your name?"
    >> getLine
    >>= (\inpStr -> putStrLn $ "Welcome to Haskell, bro " ++ inpStr ++ "!")