{-|
Module      : Main
Description : Setup script for hgeos build
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE CPP #-}

module Main (main) where

#ifdef mingw32_HOST_OS

import Data.List
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.FilePath

testPrefix :: String
testPrefix = "test:"

dllName :: String
dllName = "geos_c.dll"

main :: IO ()
main = defaultMainWithHooks hgeosHooks

hgeosHooks :: UserHooks
hgeosHooks = simpleUserHooks
    { confHook = hgeosConfHook
    , postBuild = hgeosPostBuild
    }

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

hgeosPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
hgeosPostBuild args buildFlags _ _ = do
    case find (testPrefix `isPrefixOf`) args of
         Nothing -> return ()
         Just testArg -> do
                dir <- getCurrentDirectory
                let testName = drop (length testPrefix) testArg
                    distDir = dir </> (fromFlag $ buildDistPref buildFlags)
                    buildDir = distDir </> "build"
                    testDLLPath = buildDir </> testName </> dllName
                    geosDLLPath = dir </> "external" </> "geos" </> "bin" </> dllName
                copyFile geosDLLPath testDLLPath
                return ()

#else

import Distribution.Simple

main :: IO ()
main = defaultMain

#endif