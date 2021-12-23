{-# LANGUAGE ScopedTypeVariables #-}

module ControlledVizit where

import Control.Exception (IOException, handle)
import Control.Monad (Monad (return), forM, liftM, mapM)
import Data.Time.Clock (UTCTime)
import System.Directory (Permissions (..), doesDirectoryExist, getDirectoryContents, getModificationTime, getPermissions, searchable)
import System.FilePath (FilePath, (</>))
import System.IO (IOMode (ReadMode), hFileSize, withFile)
import Prelude hiding (traverse)

data Info = Info
  { infoPath :: FilePath,
    infoPerms :: Maybe Permissions,
    infoSize :: Maybe Integer,
    infoModTime :: Maybe UTCTime
  }
  deriving (Eq, Show, Ord)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(_ :: IOException) -> return Nothing) (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (withFile path ReadMode hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $
    forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
