{-# LANGUAGE OverloadedStrings #-}
module IPS
    ( getRecords
    , isIPS
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB
import Data.Int (Int16, Int32)
import Data.Word (Word8, Word32)
import Data.Binary.Get
import Data.Bits

type Int24 = Int32
type Word24 = Word32

data IPS = IPSRecord !Int24 !Int16 !SB.ByteString
         | RLERecord !Int24 !Int16 !Word8
         deriving Show

isIPS :: B.ByteString -> Bool
isIPS bs = header == "PATCH"
    where header = B.take 5 bs

getInt24be :: Get Int24
getInt24be = do
    first  <- getWord8
    second <- getWord8
    third  <- getWord8
    pure $ fromIntegral first `shiftL` 16 .|. fromIntegral second `shiftL`  8 .|. fromIntegral third

getRecords :: B.ByteString -> [IPS]
getRecords = runGet getIPSinner . B.drop 5

getIPSinner :: Get [IPS]
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
        pure $ eof == "EOF" && empty

getRecord :: Get IPS
getRecord = do
    offset <- getInt24be
    size <- getInt16be
    if size == 0
    then do
        size <- getInt16be
        byte <- getWord8
        pure $ RLERecord offset size byte
    else do
        bytes <- getByteString $ fromIntegral size
        pure $ IPSRecord offset size bytes
