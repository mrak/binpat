{-# LANGUAGE OverloadedStrings #-}
module IPS
    ( getIPS
    , isIPS
    , patchFile
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB
import Data.Int (Int16, Int32)
import Data.Word (Word8, Word32)
import Data.Binary.Get
import Data.Bits
import Data.List (genericReplicate)
import System.IO (Handle, openTempFile, hClose, hSeek, SeekMode(AbsoluteSeek))
import System.Directory (getTemporaryDirectory, removeFile)
import Control.Exception (catch, finally)
import GHC.IO.Exception

type Int24 = Int32
type Word24 = Word32

data IPSRecord = IPSRecord Integer SB.ByteString deriving Show
--                         Offset  Data to write

isIPS :: B.ByteString -> Bool
isIPS bs = header == "PATCH"
    where header = B.take 5 bs

getInt24be :: Get Int24
getInt24be = do
    first  <- getWord8
    second <- getWord8
    third  <- getWord8
    pure $ fromIntegral first `shiftL` 16 .|. fromIntegral second `shiftL`  8 .|. fromIntegral third

getIPS:: B.ByteString -> [IPSRecord]
getIPS= runGet getIPSinner . B.drop 5

getIPSinner :: Get [IPSRecord]
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

patchFile :: [IPSRecord] -> Handle -> Handle -> IO ()
patchFile records srch dsth = do
    tmpdir <- catch getTemporaryDirectory (\(IOError _ UnsupportedOperation _ _ _ _) -> pure ".")
    (tmpfile, tmph) <- openTempFile tmpdir "eyepatch"
    finally (work records srch tmph dsth) (cleanup tmph tmpfile)
    where cleanup h f = hClose h >> removeFile f
          work r sh th dh = do
              B.hGetContents sh >>= B.hPut th
              mapM_ (patchRecord th) records
              B.hGetContents th >>= B.hPut dh
          patchRecord th (IPSRecord offset bytes) = do
              hSeek th AbsoluteSeek (fromIntegral offset)
              SB.hPut th bytes

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
