module Eyepatch.Patch where

import Eyepatch.Types
import qualified Eyepatch.Patch.IPS as IPS
import qualified Data.ByteString.Lazy as LB
import GHC.IO.Exception
import System.IO (Handle, openTempFile, hClose)
import System.Directory (getTemporaryDirectory, removeFile)
import Control.Exception (catch, finally)
import System.Environment (getProgName)
import Data.Either.Combinators (maybeToRight)

getPatch :: LB.ByteString -> Either String Patch
getPatch = maybeToRight "Unrecognized patchfile format" . IPS.tryGetPatch

patchFile :: Patch -> Handle -> Handle -> IO ()
patchFile p sh dh = do
    td <- getTempOrCurrentDir
    (tf, th) <- openTempFile td =<< getProgName
    finally (work th) (hClose th >> removeFile tf)
    where work th = do
              LB.hGetContents sh >>= LB.hPut th
              applyPatch p th
              LB.hGetContents th >>= LB.hPut dh

getTempOrCurrentDir :: IO FilePath
getTempOrCurrentDir = catch getTemporaryDirectory (\(IOError _ UnsupportedOperation _ _ _ _) -> pure ".")

applyPatch :: Patch -> Handle -> IO ()
applyPatch (IPS p) = IPS.applyPatch p
