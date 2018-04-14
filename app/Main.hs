module Main where

import Eyepatch.Arguments
import Eyepatch.Types
import Eyepatch.Patch
import qualified Data.ByteString.Lazy as LB
import Data.Either (lefts, rights)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPrint)

main :: IO ()
main = getArgs >>= parseEyepatchArgs >>= eyepatch

eyepatch :: EyepatchArgs -> IO ()
eyepatch e = do
    eithers <- mapM tryPatchParse (patchfiles e)
    let errors = lefts eithers
    let patches = rights eithers
    if null errors
       then patchFile patches (infile e) (outfile e)
       else mapM_ (\(f,e) -> hPrint stderr $ f ++ ": " ++ e) errors

tryPatchParse :: FilePath -> IO (Either (FilePath, String) Patch)
tryPatchParse f = do
    contents <- LB.readFile f
    case getPatch contents of
         Left err -> pure $ Left (f, err)
         Right p -> pure $ Right p
