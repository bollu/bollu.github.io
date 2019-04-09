{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_absint (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bollu/.cabal/bin"
libdir     = "/home/bollu/.cabal/lib/x86_64-linux-ghc-8.5.20171221/absint-0.1.0.0-4vplSXXEhTcLdXk8RUu7GR-absint"
dynlibdir  = "/home/bollu/.cabal/lib/x86_64-linux-ghc-8.5.20171221"
datadir    = "/home/bollu/.cabal/share/x86_64-linux-ghc-8.5.20171221/absint-0.1.0.0"
libexecdir = "/home/bollu/.cabal/libexec/x86_64-linux-ghc-8.5.20171221/absint-0.1.0.0"
sysconfdir = "/home/bollu/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "absint_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "absint_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "absint_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "absint_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "absint_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "absint_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
