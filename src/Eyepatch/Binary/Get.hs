module Eyepatch.Binary.Get
    ( getInt24be
    , getLEB128u
    ) where

import Eyepatch.Types
import Data.Int (Int16)
import Data.Binary.Get (Get, getWord8, runGet)
import Data.Bits (testBit, (.&.), (.|.), shiftL)
import qualified Data.ByteString.Lazy as LB

getInt24be :: Get Int24
getInt24be = do
    first  <- getWord8
    second <- getWord8
    third  <- getWord8
    pure $ fromIntegral first `shiftL` 16 .|. fromIntegral second `shiftL`  8 .|. fromIntegral third

{-| This will get a single, most-significant-bit, little-endian,
- | unsigned, variable width integer
- | See LEB128 encoding
  |-}
getLEB128u :: Get Integer
getLEB128u = getLEB128u' 0 0

getLEB128u' :: Integer -> Integer -> Get Integer
getLEB128u' o i = do
    byte <- getWord8
    let i' = i + (fromIntegral (byte .&. 0x7f) * 2^(7 * o))
    if testBit byte 7
       then getLEB128u' (o + 1) i'
       else pure i'
