module Glob (namesMatching) where

import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (fnnameL, fnnameW, matchesGlob)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getDirectoryContents,
  )
import System.FilePath (dropTrailingPathSeparator, pathSeparator, splitFileName, (</>))

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: FilePath -> IO [String]
namesMatching pat
  | not (isPattern pat) =
    do
      exists <- doesNameExists pat
      return [pat | exists]
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
        curDir <- getCurrentDirectory
        listMatches curDir baseName
      (dirName, baseName) -> do
        dirs <-
          if isPattern dirName
            then namesMatching (dropTrailingPathSeparator dirName)
            else return [dirName]
        let listDir =
              if isPattern baseName
                then listMatches
                else listPlain
        pathNames <- forM dirs $ \dir -> do
          baseNames <- listDir dir baseName
          return (map (dir </>) baseNames)
        return (concat pathNames)

-- in System.Posix.Files fileExist function
doesNameExists :: FilePath -> IO Bool
doesNameExists name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <-
    if null dirName
      then getCurrentDirectory
      else return dirName
  -- handle (const (return [""])) $ do
  names <- getDirectoryContents dirName'
  let names' =
        if isHidden pat
          then filter isHidden names
          else filter (not . isHidden) names
  return (filter (`fnname` pat) names')

isHidden ('.' : _) = True
isHidden _ = False

fnname =
  if pathSeparator == '/'
    then fnnameL
    else fnnameW

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <-
    if null baseName
      then doesDirectoryExist dirName
      else doesNameExists (dirName </> baseName)
  return [baseName | exists]
