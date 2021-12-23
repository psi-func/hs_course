module Main (main) where

import PutJson
import SimpleJSON

main :: IO ()
main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])