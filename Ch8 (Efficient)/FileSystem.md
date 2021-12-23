# Filesystem
imports
```haskell
System.Filepath
System.Directory
```
- Join two parts of filename
```haskell
    (</>) --
    "foo" </> "bar"
    -- "foo/bar"
```

- Descriptive name of function:
```haskell
 dropTrailingPathSeparator "foo/"
-- "foo"
```
- Split a path at the last slash:
```haskell
splitFileName "foo/bar/Quux.hs"
-- ("foo/bar/","Quux.hs")
splitFileName "zippity"
-- ("","zippity")
```
- Check for directory or file present in System (ungainly interface)

```haskell
doesFileExists :: FilePath -> IO Bool
doesDirectoryExist :: FilePath -> IO Bool

doesNameExists :: FilePath -> IO Bool 
doesNameExists name = do
  fileExists <- doesFileExist name
  if fileExists 
    then return True
    else doesDirectoryExist name
```

```haskell
getCurrentDirectory :: IO (String)
```
