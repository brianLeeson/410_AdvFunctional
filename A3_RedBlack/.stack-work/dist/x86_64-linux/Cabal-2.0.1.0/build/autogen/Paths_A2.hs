{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_A2 (
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

bindir     = "/home/brian/workspace/410_AdvFunctional/A3_RedBlack/.stack-work/install/x86_64-linux/lts-11.4/8.2.2/bin"
libdir     = "/home/brian/workspace/410_AdvFunctional/A3_RedBlack/.stack-work/install/x86_64-linux/lts-11.4/8.2.2/lib/x86_64-linux-ghc-8.2.2/A2-0.1.0.0-AR2U2L2vJyu6ScDncXoMy8"
dynlibdir  = "/home/brian/workspace/410_AdvFunctional/A3_RedBlack/.stack-work/install/x86_64-linux/lts-11.4/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/brian/workspace/410_AdvFunctional/A3_RedBlack/.stack-work/install/x86_64-linux/lts-11.4/8.2.2/share/x86_64-linux-ghc-8.2.2/A2-0.1.0.0"
libexecdir = "/home/brian/workspace/410_AdvFunctional/A3_RedBlack/.stack-work/install/x86_64-linux/lts-11.4/8.2.2/libexec/x86_64-linux-ghc-8.2.2/A2-0.1.0.0"
sysconfdir = "/home/brian/workspace/410_AdvFunctional/A3_RedBlack/.stack-work/install/x86_64-linux/lts-11.4/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "A2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "A2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "A2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "A2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "A2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "A2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
