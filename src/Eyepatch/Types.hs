module Eyepatch.Types where

import Data.Int (Int32)
import Data.Word (Word32)
import qualified Data.ByteString as SB (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.ByteString (ByteString)
import System.IO (Handle)

type Int24 = Int32
type Word24 = Word32
type IPSPatch = [IPSRecord]

--                         Offset  Data to write
data IPSRecord = IPSRecord Integer SB.ByteString deriving Show

data Patch = IPS IPSPatch

data EyepatchArgs = EyepatchArgs { infile    :: FilePath
                                 , patchfile :: FilePath
                                 , outfile   :: FilePath
                                 }
