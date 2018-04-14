{-# LANGUAGE QuasiQuotes #-}
module Eyepatch.Arguments where

import Eyepatch.Types
import Data.Maybe (fromMaybe)
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
    eyepatch [options] [--] <file> <patchfile>
    eyepatch <file> <patchfile> [options]

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

    let outfile = fromMaybe "-" $ getArg args (shortOption 'o')

    pure $ EyepatchArgs infile patchfile outfile

readFileOrStdin :: FilePath -> IO Handle
readFileOrStdin "-" = pure stdin
readFileOrStdin fp = openBinaryFile fp ReadMode
