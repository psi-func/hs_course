module Prettify where

import Prelude hiding ((<>))

data Doc
  = Empty
  | Text String
  | Line
  | Char Char
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show)

text :: String -> Doc
text "" = Empty
text t = Text t

empty :: Doc
empty = Empty

double :: Double -> Doc
double = text . show

char :: Char -> Doc
char = Char

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
y <> Empty = y
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softLine <> y

softLine :: Doc
softLine = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d <> p) : punctuate p ds

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n' : _) = True
w `fits` (c : cs) = (w - 1) `fits` cs

-- fill :: Int -> Doc -> Doc
-- fill

compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (d : ds) =
      case d of
        Empty -> transform ds
        Char c -> c : transform ds
        Text s -> s ++ transform ds
        Line -> '\n' : transform ds
        a `Concat` b -> transform (a : b : ds)
        _ `Union` b -> transform (b : ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where
    best col (d : ds) =
      case d of
        Empty -> best col ds
        Char c -> c : best (col + 1) ds
        Text s -> s ++ best (col + length s) ds
        Line -> '\n' : best 0 ds
        a `Concat` b -> best col (a : b : ds)
        a `Union` b -> nicest col (best col (a : ds)) (best col (b : ds))
    best _ _ = ""

    nicest col a b
      | (width - least) `fits` a = a
      | otherwise = b
      where
        least = min width col
