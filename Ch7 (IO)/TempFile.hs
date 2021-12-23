module TempFile where

import Control.Exception (finally)
import Data.Char (digitToInt, toLower)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import System.IO.Error (catchIOError)

main :: IO ()
main = withTempFile "myTemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
  do
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temporary file at " ++ tempname

    -- See initial position
    pos <- hTell temph
    putStrLn $ "My initial position is " ++ show pos

    --  Write some data to the temporary file
    let tempdata = show [1 .. 10]
    putStrLn $
      "Writing one line containing "
        ++ show (length tempdata)
        ++ " bytes: "
        ++ tempdata
    hPutStrLn temph tempdata

    -- Get our new position
    pos <- hTell temph
    putStrLn $ "After writing my new position is " ++ show pos

    -- Seek to the beginning of the file and display it
    putStrLn "The file content is:"
    hSeek temph AbsoluteSeek 0
    c <- hGetContents temph

    -- Copy byte by byte
    putStrLn c

    -- Literals
    putStrLn "Which could be expressed as this Haskell literal:"
    print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pat func =
  do
    tempdir <- catchIOError getTemporaryDirectory (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir pat

    finally
      (func tempfile temph)
      ( do
          hClose temph
          removeFile tempfile
      )
