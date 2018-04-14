module Eyepatch.Patch where

import Eyepatch.Types
import qualified Eyepatch.Patch.IPS as IPS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import GHC.IO.Exception
import System.IO (Handle, openBinaryTempFile, hClose)
import System.Directory (copyFile, getTemporaryDirectory, removeFile)
import Control.Exception (catch, finally)
import System.Environment (getProgName)
import Data.Either.Combinators (maybeToRight)

getPatch :: LB.ByteString -> Either String Patch
getPatch = maybeToRight "Unrecognized patchfile format" . IPS.tryGetPatch

patchFile :: [Patch] -> FilePath -> FilePath -> IO ()
patchFile ps s d = do
    td <- getTempOrCurrentDir
    (tf, th) <- openBinaryTempFile td =<< getProgName
    finally (work tf th) (hClose th >> removeFile tf)
    where work tf th = do
              copySource s th
              mapM_ (`applyPatch` th) ps
              hClose th
              copyDestination tf d

copySource :: FilePath -> Handle -> IO ()
copySource "-" h = SB.getContents >>= SB.hPut h
copySource f h = SB.readFile f >>= SB.hPut h

copyDestination :: FilePath -> FilePath -> IO ()
copyDestination s "-" = SB.readFile s >>= SB.putStr
copyDestination s d = copyFile s d

getTempOrCurrentDir :: IO FilePath
getTempOrCurrentDir = catch getTemporaryDirectory (\(IOError _ UnsupportedOperation _ _ _ _) -> pure ".")

applyPatch :: Patch -> Handle -> IO ()
applyPatch (IPS p) = IPS.applyPatch p
