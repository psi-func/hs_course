# I/O Lib

---

```haskell
import Control.Monad
forM :: (Monad m) => [a] -> (a -> m b) -> m [b]

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]

```

just flipped mapM

```haskell
import System.FilePath

takeExtension :: FilePath -> String
takeExtension "foo/bar.c" -- ".c"

```

```haskell
import System.Directory

doesFileExists :: FilePath -> IO Bool
doesDirectoryExist :: FilePath -> IO Bool

getPermissions :: FilePath -> IO Permissions

data Permissions
= Permissions {readable :: Bool,
writable :: Bool,
executable :: Bool,
searchable :: Bool}
-- Defined in System.Directory
instance Eq Permissions -- Defined in System.Directory
instance Ord Permissions -- Defined in System.Directory
instance Read Permissions -- Defined in System.Directory
instance Show Permissions -- Defined in System.Directory

getModificationTime :: FilePath -> IO System.Time.CLockTime
-- see unix compat
```
