module Main where

import IPS
import qualified Data.ByteString.Lazy as B
import System.Exit
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    contents <- B.readFile $ head args
    if isIPS contents
    then do
        print $ getRecords contents
        exitSuccess
    else exitFailure
