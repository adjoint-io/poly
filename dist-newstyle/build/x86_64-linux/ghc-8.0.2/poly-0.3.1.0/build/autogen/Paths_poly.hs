{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_poly (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,3,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/alberto/.cabal/bin"
libdir     = "/home/alberto/.cabal/lib/x86_64-linux-ghc-8.0.2/poly-0.3.1.0-inplace"
dynlibdir  = "/home/alberto/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/alberto/.cabal/share/x86_64-linux-ghc-8.0.2/poly-0.3.1.0"
libexecdir = "/home/alberto/.cabal/libexec/x86_64-linux-ghc-8.0.2/poly-0.3.1.0"
sysconfdir = "/home/alberto/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "poly_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "poly_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "poly_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "poly_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "poly_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "poly_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
