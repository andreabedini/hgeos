{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Geolocation.GEOS
import qualified GEOSTest.HighLevelAPI as HighLevelAPI
import qualified GEOSTest.LowLevelAPI as LowLevelAPI
import qualified GEOSTest.Sample as Sample

main :: IO ()
main = do
    v <- version
    putStrLn v
    LowLevelAPI.demo
    HighLevelAPI.demo
    Sample.demo
