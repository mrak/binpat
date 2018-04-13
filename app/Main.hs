{-# LANGUAGE QuasiQuotes #-}
module Main where

import Eyepatch.Types
import Eyepatch.Patch
import qualified Data.ByteString.Lazy as LB
import System.Exit
import System.Environment
import System.Console.Docopt
import System.IO ( stdout
                 , stderr
                 , Handle
                 , openBinaryFile
                 , hPutStrLn
                 , IOMode( ReadMode
                         , WriteMode
                         )
                 )
import System.Directory

patterns :: Docopt
patterns = [docopt|
Usage:
    eyepatch <file> <patchfile>

Options:
    -o=<file>   Output to file. Defaults to STDOUT
|]

data EyepatchArgs = EyepatchArgs { infile :: Handle
                                 , patchfile :: Handle
                                 , outfile :: Handle }

main :: IO ()
main = getArgs >>= parseArgsOrExit patterns >>= parseEyepatchArgs >>= eyepatch

parseEyepatchArgs :: Arguments -> IO EyepatchArgs
parseEyepatchArgs args = do
    infile <- getArgOrExitWith patterns args (argument "file")
    patchfile <- getArgOrExitWith patterns args (argument "patchfile")
    let mOutfile = getArg args (shortOption 'o')

    hInfile <- openBinaryFile infile ReadMode
    hPatchfile <- openBinaryFile patchfile ReadMode
    hOutfile <- case mOutfile of
                     Nothing -> pure stdout
                     Just o -> openBinaryFile o WriteMode

    pure $ EyepatchArgs hInfile hPatchfile hOutfile

eyepatch :: EyepatchArgs -> IO ()
eyepatch e = do
    contents <- LB.hGetContents $ patchfile e
    case getPatch contents of
         Left e -> hPutStrLn stderr e >> exitFailure
         Right p -> patchFile p (infile e) (outfile e)
