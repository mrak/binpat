{-# LANGUAGE OverloadedStrings #-}
module Eyepatch.Patch.IPS
    ( tryGetPatch
    , applyPatch
    ) where

import Eyepatch.Types
import Eyepatch.Binary.Get (getInt24be)

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import Control.Monad (guard)
import Data.Int (Int16)
import Data.Word (Word8, Word32)
import Data.Binary.Get ( Get
                       , getWord8
                       , runGet
                       , lookAhead
                       , getByteString
                       , getInt16be
                       , isEmpty
                       )
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

getIPS:: LB.ByteString -> Maybe IPSPatch
getIPS = Just <$> runGet getIPS' . LB.drop 5

getIPS' :: Get IPSPatch
getIPS' = do
    finished <- lookAhead eofCheck
    if finished
       then pure []
       else do
           record <- getRecord
           rest <- getIPS'
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
