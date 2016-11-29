{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_christmalt_cabal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ben/.cabal/bin"
libdir     = "/home/ben/.cabal/lib/x86_64-linux-ghc-7.10.3/christmalt-cabal-0.1.0.0-Fe39mOcrdzq1IQRyIVQQvS"
datadir    = "/home/ben/.cabal/share/x86_64-linux-ghc-7.10.3/christmalt-cabal-0.1.0.0"
libexecdir = "/home/ben/.cabal/libexec"
sysconfdir = "/home/ben/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "christmalt_cabal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "christmalt_cabal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "christmalt_cabal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "christmalt_cabal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "christmalt_cabal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
