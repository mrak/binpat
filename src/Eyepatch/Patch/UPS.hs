{-# LANGUAGE OverloadedStrings #-}
module Eyepatch.Patch.UPS
    ( tryGetPatch
    , applyPatch
    ) where

import Eyepatch.Types
import Eyepatch.Binary.Get

import qualified Data.ByteString.Lazy as LB
import Control.Monad (guard)
import System.IO (Handle)
import Data.Word (Word32)
import Data.Digest.CRC32 (crc32)
import Data.Binary.Get ( Get
                       , runGet
                       , getWord32le
                       , isEmpty
                       , getLazyByteStringNul
                       )

header = "UPS1"

applyPatch :: UPSPatch -> Handle -> IO ()
applyPatch = do
    -- verify source file checksum
    undefined
    -- verify output file checksum

tryGetPatch :: LB.ByteString -> Maybe Patch
tryGetPatch bs = UPS <$> getUPS bs

getUPS :: LB.ByteString -> Maybe UPSPatch
getUPS bs = do
    let (h, r) = LB.splitAt 4 bs
    if h /= header
        then Nothing
        else do
            let (p, cs) = LB.splitAt (LB.length bs - (4 * 3)) r
            let checksums@(iSum, oSum, pSum) = runGet getChecksums cs
            if crc32 (LB.take (LB.length bs - 4) bs) == pSum
               then Just $ runGet (getUPS' checksums) p
               else Nothing

getChecksums :: Get (Word32, Word32, Word32)
getChecksums = do
    iSum <- getWord32le
    oSum <- getWord32le
    pSum <- getWord32le
    pure (iSum, oSum, pSum)

getUPS' :: (Word32, Word32, Word32) -> Get UPSPatch
getUPS' (is, os, ps) = do
    iSize <- getLEB128u
    oSize <- getLEB128u
    xorBlocks <- getXORblocks
    pure $ UPSPatch iSize oSize xorBlocks is os ps

getXORblocks :: Get [XORblock]
getXORblocks = do
    empty <- isEmpty
    if empty
       then pure []
       else do
          skip <- getLEB128u
          xorBlock <- LB.toStrict <$> getLazyByteStringNul
          rest <- getXORblocks
          pure $ (skip, xorBlock) : rest
