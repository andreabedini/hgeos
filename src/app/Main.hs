{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Geolocation.GEOS.Imports
import Foreign.C
import qualified HighLevelAPI
import qualified LowLevelAPI
import qualified Sample

main :: IO ()
main = do
    s <- peekCString c_GEOSversion
    putStrLn s
    LowLevelAPI.demo
    HighLevelAPI.demo
    Sample.demo
