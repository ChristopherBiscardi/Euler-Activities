module Paths_three (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/chris/bin"
libdir     = "/Users/chris/lib"
datadir    = "/Users/chris/share"
libexecdir = "/Users/chris/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "three_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "three_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "three_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "three_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
