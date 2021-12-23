# Efficient File Proceccing
```haskell
main = do
  contents <- getContents
  print $ sumFile contents
  where
    sumFile = sum . map read . words
```

unefficient code

## Bytestring usage -> C efficienty

```haskell
Data.ByteString
Data.ByteString.Lazy
```

- Data ByteString defines strict type which represents a string in a single array
- Lazy type also represents aa string data as a list of *chunks*, arrays of up to 64 KB in size

# Binary I/O
**Word8** type to represent byte
```haskell
import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where
    elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]
```

**Note :** *qualified* import let refer to module with a name chosen

A lot of function with the same name

# Text I/O
```haskell
Data.ByteString.Char8
Data.ByteString.Lazy.Char8
```
indovidual element as **Char** instead of **Word8**

**Note:** suitablew only with ASCII chars. Values above 255 are truncated

# Filename Matching
glob patterns as regular expressions

## RegExp
```haskell
Text.Regex.Posix
```
Regex match, like Perl:
```haskell 
(=~)
```
Heavy use of polymorphism.  
Need to use ``String`` or *strict* ``Bytestring``

```haskell
  -- results for Bool
 "my left foot" =~ "foo" :: Bool
  --  True
  "your right hand" =~ "(hand|foot)" :: Bool
  -- True

   "a star called henry" =~ "planet" :: Int
  -- 0
  "honorificabilitudinitatibus" =~ "[aeiou]" :: Int
  --  13

  "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: String
  --  "ii"
  "hi ludi, F. Baconis nati, tuiti orbi" =~ "Shakespeare" :: String
  -- ""

   "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: [String]
  -- ["ii","uu"]

   "hi ludi, F. Baconis nati, tuiti orbi" =~ "Shakespeare" ::[String]
  -- []

```

type for before, match and after ``(String, String, String)``

``(String, String, String, [String])`` 4th param for match list

``(Int, Int)`` and ``[(Int Int)]``

try **regex-tdfa**