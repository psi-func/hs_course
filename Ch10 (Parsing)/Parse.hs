{-# LANGUAGE LambdaCase #-}

module Parse where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

data ParseState = ParseState
  { string :: L.ByteString,
    offset :: Int64
  }
  deriving (Show)

newtype Parse a = Parse
  { runParse :: ParseState -> Either String (a, ParseState)
  }

instance Functor Parse where
  fmap f parser = parser ==> \result -> identity (f result)

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
  case runParse parser (ParseState initState 0) of
    Left err -> Left err
    Right (result, _) -> Right result

bail :: String -> Parse a
bail err = Parse $ \s ->
  Left $
    "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where
    chainedParser initState = case runParse firstParser initState of
      Left errMessage -> Left errMessage
      Right (firstResult, newState) -> runParse (secondParser firstResult) newState

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

parseChar :: Parse Char
parseChar = w2c <$> parseByte

parseByte :: Parse Word8
parseByte =
  getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
        bail "no more input"
      Just (byte, remainder) ->
        putState newState ==> \_ ->
          identity byte
        where
          newState =
            initState
              { string = remainder,
                offset = newOffset
              }
          newOffset = offset initState + 1

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState {offset = newOffset}

w2c :: Word8 -> Char
w2c = chr . fromIntegral

peekByte :: Parse (Maybe Word8)
peekByte = fmap fst . L.uncons . string <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p =
  (fmap p <$> peekByte) ==> \mp ->
    if mp == Just True
      then parseByte ==> \b -> (b :) <$> parseWhile p
      else identity []

parseWhileVerbose :: (Word8 -> Bool) -> Parse [Word8]
parseWhileVerbose p =
  peekByte ==> \case
    Nothing -> identity []
    Just c
      | p c ->
        parseByte ==> \b ->
          parseWhileVerbose p ==> \bs ->
            identity (b : bs)
      | otherwise ->
        identity []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat =
  parseWhileWith w2c isDigit ==> \digits ->
    if null digits
      then bail "no more input"
      else
        let n = read digits
         in if n < 0
              then bail "integer overflow"
              else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> const f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

-- simpleParse :: ParseState -> (a, ParseState)
-- simpleParse = undefined

-- betterParse :: ParseState -> Either String (a, ParseState)
-- betterParse = undefined
