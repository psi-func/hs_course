module GlobRegex
  ( globToRegex,
    matchesGlob,
    fnnameL,
    fnnameW,
  )
where

import Data.Char (isLower, isUpper, toLower, toUpper)
import Text.Regex.Posix ((=~))

globToRegex :: Bool -> String -> String
globToRegex s cs = '^' : globToRegex' s cs ++ "$"

globToRegex' :: Bool -> String -> String
globToRegex' _ "" = ""
globToRegex' s ('*' : cs) = ".*" ++ globToRegex' s cs
globToRegex' s ('?' : cs) = '.' : globToRegex' s cs
globToRegex' s ('[' : '!' : c : cs) =
  let ch = if s then [c] else noSensCase c
   in "[^" ++ ch ++ charClass s cs
globToRegex' s ('[' : c : cs) =
  let ch = if s then [c] else noSensCase c
   in '[' : ch ++ charClass s cs
globToRegex' _ ('[' : _) = error "unterminated character class"
globToRegex' s (c : cs) = escape s c ++ globToRegex' s cs

escape :: Bool -> Char -> String
escape s c
  | c `elem` regexChars = '\\' : [c]
  | otherwise = if s then [c] else noSensCase c
  where
    regexChars = "\\+()^$.{}]|"

charClass :: Bool -> String -> String
charClass s (']' : cs) = ']' : globToRegex' s cs
charClass s (c : cs) =
  let ch = if s then [c] else noSensCase c
   in c : charClass s cs
charClass _ [] = error "unterminated character class"

noSensCase :: Char -> String
noSensCase x = if toLower x `elem` ['a' .. 'z'] then ['[', x, toggleChar x, ']'] else [x]

toggleChar :: Char -> Char
toggleChar x
  | isUpper x = toLower x
  | isLower x = toUpper x
  | otherwise = x

matchesGlob :: Bool -> FilePath -> String -> Bool
matchesGlob s name pat = name =~ globToRegex s pat

fnnameW = matchesGlob False

fnnameL = matchesGlob True