module Main where

import Eyepatch.Arguments
import Eyepatch.Types
import Eyepatch.Patch
import qualified Data.ByteString.Lazy as LB
import Data.Either (lefts)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPrint)

main :: IO ()
main = getArgs >>= parseEyepatchArgs >>= eyepatch

eyepatch :: EyepatchArgs -> IO ()
eyepatch e = mapM_ perFile (patchfiles e) where
    perFile f = either error patch . getPatch <$> LB.readFile f
    error = hPrint stderr
    patch p = patchFile p (infile e) (outfile e)
