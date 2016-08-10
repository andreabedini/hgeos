{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Geolocation.GEOS
import qualified GEOSTest.HighLevelAPI1 as HighLevelAPI1
import qualified GEOSTest.HighLevelAPI2 as HighLevelAPI2
import qualified GEOSTest.LowLevelAPI as LowLevelAPI
import qualified GEOSTest.Sample as Sample

main :: IO ()
main = do
    v <- version
    putStrLn v
    LowLevelAPI.demo
    HighLevelAPI1.demo
    HighLevelAPI2.demo
    Sample.demo
