{-# LANGUAGE OverloadedStrings #-}
module Eyepatch.Patch.UPS
    ( tryGetPatch
    , applyPatch
    ) where

import Eyepatch.Types

import qualified Data.ByteString.Lazy as LB
import System.IO (Handle)

header = "UPS1"

applyPatch :: IPSPatch -> Handle -> IO ()
applyPatch = undefined

tryGetPatch :: LB.ByteString -> Maybe Patch
tryGetPatch = undefined

isUPS :: LB.ByteString -> Bool
isUPS bs =  LB.take 4 bs == header
