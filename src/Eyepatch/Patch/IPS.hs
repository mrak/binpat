{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Eyepatch.Patch.IPS
    ( tryGetPatch
    , applyPatch
    ) where

import Eyepatch.Types

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import Control.Monad (guard)
import Data.Int (Int16, Int32)
import Data.Word (Word8, Word32)
import Data.Binary.Get ( Get
                       , getWord8
                       , runGet
                       , lookAhead
                       , getByteString
                       , getInt16be
                       , isEmpty
                       )
import Data.Bits ((.|.), shiftL)
import Data.List (genericReplicate)
import System.IO (Handle, hSeek, SeekMode(AbsoluteSeek))

header = "PATCH"
footer = "EOF"

applyPatch :: IPSPatch -> Handle -> IO ()
applyPatch p h = mapM_ (patchRecord h) p
    where patchRecord th (IPSRecord offset bytes) = do
            hSeek th AbsoluteSeek (fromIntegral offset)
            SB.hPut th bytes

tryGetPatch :: LB.ByteString -> Maybe Patch
tryGetPatch bs = do
    guard (isIPS bs)
    IPS <$> getIPS bs

isIPS :: LB.ByteString -> Bool
isIPS bs =  LB.take 5 bs == header

getInt24be :: Get Int24
getInt24be = do
    first  <- getWord8
    second <- getWord8
    third  <- getWord8
    pure $ fromIntegral first `shiftL` 16 .|. fromIntegral second `shiftL`  8 .|. fromIntegral third

getIPS:: LB.ByteString -> Maybe IPSPatch
getIPS = Just <$> runGet getIPSinner . LB.drop 5

getIPSinner :: Get IPSPatch
getIPSinner = do
    finished <- lookAhead eofCheck
    if finished
       then pure []
       else do
           record <- getRecord
           rest <- getIPSinner
           pure $ record : rest

eofCheck :: Get Bool
eofCheck = do
        eof <- getByteString 3
        empty <- isEmpty
        pure $ eof == footer && empty

getRecord :: Get IPSRecord
getRecord = do
    offset <- getInt24be
    size <- getInt16be
    if size == 0
    then do
        size <- getInt16be
        byte <- getWord8
        pure $ IPSRecord (fromIntegral offset) (SB.pack.genericReplicate size $ byte)
    else do
        bytes <- getByteString $ fromIntegral size
        pure $ IPSRecord (fromIntegral offset) bytes
