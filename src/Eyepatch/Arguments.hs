{-# LANGUAGE QuasiQuotes #-}
module Eyepatch.Arguments where

import Eyepatch.Types
import System.Console.Docopt
import Control.Monad (when)
import System.Exit (exitFailure)
import System.IO ( Handle
                 , hPrint
                 , stderr
                 , stdin
                 , stdout
                 , openBinaryFile
                 , IOMode( ReadMode
                         , WriteMode
                         )
                 )

patterns :: Docopt
patterns = [docopt|
Usage:
    eyepatch <file> <patchfile>
    eyepatch -      <patchfile>
    eyepatch <file> -

Options:
    -o=<file>   Output to file. Defaults to STDOUT
|]

parseEyepatchArgs :: [String] -> IO EyepatchArgs
parseEyepatchArgs rawArgs = do
    args <- parseArgsOrExit patterns rawArgs
    infile <- getArgOrExitWith patterns args (argument "file")
    patchfile <- getArgOrExitWith patterns args (argument "patchfile")

    when ("-" == infile && "-" == patchfile) $ do
        hPrint stderr "Only one of <file> and <patchfile> can be STDIN"
        exitFailure

    let mOutfile = getArg args (shortOption 'o')

    hInfile <- readFileOrStdin infile
    hPatchfile <- readFileOrStdin patchfile
    hOutfile <- case mOutfile of
                     Nothing -> pure stdout
                     Just o -> openBinaryFile o WriteMode

    pure $ EyepatchArgs hInfile hPatchfile hOutfile

readFileOrStdin :: FilePath -> IO Handle
readFileOrStdin "-" = pure stdin
readFileOrStdin fp = openBinaryFile fp ReadMode
