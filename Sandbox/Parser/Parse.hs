module Parse where

import qualified Control.Exception as L
import Data.Binary (Word8)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import PNM (Greymap (Greymap))

data ParseState = ParseState
  { string :: L.ByteString,
    offset :: Int64 -- Imported from Data.Int
  }
  deriving (Show)

newtype Parse a = Parse
  { runParse :: ParseState -> Either String (a, ParseState)
  }

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = case runParse parser (ParseState initState 0) of
  Left err -> Left err
  Right (result, _) -> Right result

-- partly change
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState {offset = newOffset}

getState :: Parse ParseState
getState = Parse $ \s -> Right (s, s)

putState :: ParseState -> Parse ()
putState s = Parse $ \_ -> Right ((), s)

(==>) :: Parse a -> (a -> Parse b) -> Parse b
f ==> sec = Parse chainedParser
  where
    chainedParser initState = case runParse f initState of
      Left err -> Left err
      Right (res, st') -> runParse (sec res) st'

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> const f

instance Functor Parse where
  fmap f parser = parser ==> \result -> identity (f result)

bail :: String -> Parse a
bail err =
  Parse $ \s -> Left $ "byte offset" ++ show (offset s) ++ ": " ++ err

identity :: a -> Parse a
identity a = Parse $ \s -> Right (a, s)

parseByte :: Parse Word8
parseByte =
  getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing -> bail "no more input"
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

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = fmap fst . L.uncons . string <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p =
  (fmap p <$> peekByte) ==> \mp ->
    if mp == Just True
      then
        parseByte ==> \b ->
          (b :) <$> parseWhile p
      else identity []

parseWhileWith :: (Word8 -> a) -> (Word8 -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile p

parseNat :: Parse Int
parseNat =
  parseWhileWith w2c (isDigit . w2c) ==> \digits ->
    if null digits
      then bail "no more input"
      else
        let n = read digits
         in if n < 0
              then bail "integer overflow"
              else identity n

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st {offset = offset st + L.length h, string = t}
     in putState st'
          ==>& assert (L.length h == n') "end of input"
          ==>& identity h

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c (isSpace . w2c) ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

parseRawPGM =
  parseWhileWith w2c (notWhite . w2c) ==> \header ->
    skipSpaces
      ==>& assert (header == "P5") "invalid raw header"
      ==>& parseNat ==> \width ->
        skipSpaces
          ==>& parseNat ==> \height ->
            skipSpaces
              ==>& parseNat ==> \maxgrey ->
                parseByte
                  ==>& assert (maxgrey < 65535) "too much"
                  ==>& let n' = if maxgrey < 256 then 1 else 2
                        in parseBytes (width * height * n') ==> \bitmap ->
                             identity (Greymap width height maxgrey bitmap)
  where
    notWhite = (`notElem` " \r\n\t")
