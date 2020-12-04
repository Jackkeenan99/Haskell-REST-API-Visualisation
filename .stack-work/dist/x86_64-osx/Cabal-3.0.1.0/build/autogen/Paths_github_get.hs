{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_github_get (
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

bindir     = "/Users/jackkeenan/Documents/Sweng/Haskell-REST-API-Visualisation/.stack-work/install/x86_64-osx/e33894976b30ac835741fc3d7ad600673430133765f20eff3376e0f199706545/8.8.4/bin"
libdir     = "/Users/jackkeenan/Documents/Sweng/Haskell-REST-API-Visualisation/.stack-work/install/x86_64-osx/e33894976b30ac835741fc3d7ad600673430133765f20eff3376e0f199706545/8.8.4/lib/x86_64-osx-ghc-8.8.4/github-get-0.1.0.0-6bRlghxQ3pW3p24PS2PYGu"
dynlibdir  = "/Users/jackkeenan/Documents/Sweng/Haskell-REST-API-Visualisation/.stack-work/install/x86_64-osx/e33894976b30ac835741fc3d7ad600673430133765f20eff3376e0f199706545/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/jackkeenan/Documents/Sweng/Haskell-REST-API-Visualisation/.stack-work/install/x86_64-osx/e33894976b30ac835741fc3d7ad600673430133765f20eff3376e0f199706545/8.8.4/share/x86_64-osx-ghc-8.8.4/github-get-0.1.0.0"
libexecdir = "/Users/jackkeenan/Documents/Sweng/Haskell-REST-API-Visualisation/.stack-work/install/x86_64-osx/e33894976b30ac835741fc3d7ad600673430133765f20eff3376e0f199706545/8.8.4/libexec/x86_64-osx-ghc-8.8.4/github-get-0.1.0.0"
sysconfdir = "/Users/jackkeenan/Documents/Sweng/Haskell-REST-API-Visualisation/.stack-work/install/x86_64-osx/e33894976b30ac835741fc3d7ad600673430133765f20eff3376e0f199706545/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "github_get_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "github_get_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "github_get_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "github_get_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "github_get_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "github_get_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
