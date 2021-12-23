{-# LANGUAGE ScopedTypeVariables #-}

module BetterPredicate where

import Control.Exception (IOException, bracket, handle)
import Control.Monad (filterM)
import Data.Time.Clock (UTCTime)
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), hClose, hFileSize, openFile, withFile)

type InfoP a =
  FilePath -> -- path to directory entry
  Permissions -> -- permissions
  Maybe Integer -> -- file size
  UTCTime -> -- last modified
  a

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: IOException) -> return Nothing) $
  withFile path ReadMode $ \h -> do
    --like bracket (openFile path ReadMode) hClose ...
    size <- hFileSize h
    return (Just size)

betterFind :: InfoP Bool -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return (p name perms size modified)

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP

infix 4 ==?

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

infixr 3 &&?

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP

infix 4 >?

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest3 =
  (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

myTest4 =
  liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

myTest2 =
  (liftPath takeExtension `equalP` ".cpp")
    `andP` (sizeP `greaterP` 131072)

myTest path _ (Just size) _ =
  takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(_ :: IOException) -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)