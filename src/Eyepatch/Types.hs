module Eyepatch.Types where

import Data.Int (Int32)
import Data.Word (Word32)
import Data.ByteString (ByteString)

type Int24 = Int32
type Word24 = Word32

data IPSRecord = IPSRecord Integer ByteString deriving Show
--                         Offset  Data to write
