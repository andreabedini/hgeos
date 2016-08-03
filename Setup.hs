{-|
Module      : Main
Description : Setup script for hgeos build
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE CPP #-}

module Main (main) where

#ifdef mingw32_HOST_OS

import Distribution.PackageDescription
    ( GenericPackageDescription
    , HookedBuildInfo
    , extraLibDirs
    , includeDirs
    , libBuildInfo
    , library
    )
import Distribution.Simple
    ( confHook
    , defaultMainWithHooks
    , simpleUserHooks
    )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , configFlags
    , localPkgDescr
    )
import Distribution.Simple.Setup (ConfigFlags)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = hgeosConfHook }

hgeosConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
hgeosConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        Just packageLibrary = library packageDescription
        libraryBuildInfo = libBuildInfo packageLibrary

    dir <- getCurrentDirectory
    let externalDir = dir </> "external"
        geosPackageDir = externalDir </> "geos"
        geosIncludeDir = geosPackageDir </> "include"
        geosLibDir = geosPackageDir </> "lib"

    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ packageLibrary {
                libBuildInfo = libraryBuildInfo {
                    includeDirs = geosIncludeDir : includeDirs libraryBuildInfo,
                    extraLibDirs = geosLibDir : extraLibDirs libraryBuildInfo
                }
            }
        }
    }

#else

import Distribution.Simple

main :: IO ()
main = defaultMain

#endif
