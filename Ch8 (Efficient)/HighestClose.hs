module HighestClose where

import qualified Data.ByteString.Lazy.Char8 as L
import Text.Regex.Posix
import Prelude hiding (readFile)

closing = readFile . (!! 4) . L.split ','

readFile :: L.ByteString -> Maybe Int
readFile str =
  case L.readInt str of
    Nothing -> Nothing
    Just (dollars, rest) ->
      case L.readInt (L.tail rest) of
        Nothing -> Nothing
        Just (cents, more) ->
          Just (dollars * 100 + cents)

highestClose = maximum . (Nothing :) . map closing . L.lines

highestCloseFrom path = do
  contents <- L.readFile path
  print (highestClose contents)