{-# LANGUAGE QuasiQuotes #-}
module Main where

import Eyepatch.Format.IPS
import qualified Data.ByteString.Lazy as B
import System.Exit
import System.Environment
import System.Console.Docopt
import System.IO (stdout, Handle, openBinaryFile, IOMode(ReadMode,WriteMode))
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
    contents <- B.hGetContents $ patchfile e
    if isIPS contents
    then do
        let records = getIPS contents
        patchFile records (infile e) (outfile e)
        print records
        exitSuccess
    else exitFailure
