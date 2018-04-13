module Main where

import Eyepatch.Arguments
import Eyepatch.Types
import Eyepatch.Patch
import qualified Data.ByteString.Lazy as LB
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPrint)

main :: IO ()
main = getArgs >>= parseEyepatchArgs >>= eyepatch

eyepatch :: EyepatchArgs -> IO ()
eyepatch e = do
    contents <- LB.hGetContents $ patchfile e
    case getPatch contents of
         Left e -> hPrint stderr e >> exitFailure
         Right p -> patchFile p (infile e) (outfile e)
