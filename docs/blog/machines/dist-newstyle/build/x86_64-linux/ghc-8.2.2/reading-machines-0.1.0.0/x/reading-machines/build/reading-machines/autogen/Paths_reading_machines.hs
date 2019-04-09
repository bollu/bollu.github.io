{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_reading_machines (
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
libdir     = "/home/bollu/.cabal/lib/x86_64-linux-ghc-8.2.2/reading-machines-0.1.0.0-inplace-reading-machines"
dynlibdir  = "/home/bollu/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/bollu/.cabal/share/x86_64-linux-ghc-8.2.2/reading-machines-0.1.0.0"
libexecdir = "/home/bollu/.cabal/libexec/x86_64-linux-ghc-8.2.2/reading-machines-0.1.0.0"
sysconfdir = "/home/bollu/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "reading_machines_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "reading_machines_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "reading_machines_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "reading_machines_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "reading_machines_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "reading_machines_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
