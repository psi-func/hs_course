module ElfMagic where

import qualified Data.ByteString.Lazy as L
import Data.Graph (path)

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where
    elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfMagic :: FilePath -> IO Bool
isElfMagic path = do
  content <- L.readFile path
  return (hasElfMagic content)
