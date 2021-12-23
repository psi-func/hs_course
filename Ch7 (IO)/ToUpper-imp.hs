module ToUpper where

import Data.Char (toUpper)
import qualified System.Console.Terminfo as System.Console
import System.IO

-- ReadMode r-- Begin of file <-> Must exist already
-- WriteMode -w- Begin of file <-> File is truncated if exist
-- ReadWriteMode rw- Begin of file <-> File is created if it didn't exist, otherwise , existing data is left intact
-- AppendMode -w- End of file <-> Created id didn't exist; otherwise, existing data is left intact

-- FOR binary files use openBinaryFile (important on Windows )
-- on Linux it is the same operations

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  mainLoop inh outh
  hClose inh
  hClose outh

mainLoop :: Handle -> Handle -> IO ()
mainLoop inh outh =
  do
    ineof <- hIsEOF inh
    if ineof
      then return ()
      else do
        inpStr <- hGetLine inh
        hPutStrLn outh (map toUpper inpStr)
        mainLoop inh outh

-- hTell <- tells your pos in file
-- hSeek <- change position
-- SeekMode: AbsoluteSeek, RelativeSeek, SeekFromEnd (not all seekable) may be network or tape drivers or terminals (hIsSeekable)

-- stdin stdout stderr <- Handles

-- removeFile | renameFile from System.Directory

-- Can openTempFile (openBinaryTempFile) <- work with temporary files (DIRECTORY System.Directory.getTemporaryDirectory)

-- BUFFERING
-- NoBuffering, LineBuffering, BlockBuffering <Maybe <size>>
-- hSetBuffering <Handle> <BufferMode>, hGetBuffering

-- hFlush or hClose to force buffering

-- COMMAND LINE ARGS
-- System.Console.GetOpt <- parse options
-- system.Environment.getArgs
-- System.Environment.getProgName

-- Environment variables
-- System.Environment: getEnv or getEnvironment
