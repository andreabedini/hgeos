{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Geolocation.GEOS
import Foreign.C
import qualified HighLevelAPI
import qualified LowLevelAPI
import qualified Sample

main :: IO ()
main = do
    v <- version
    putStrLn v
    LowLevelAPI.demo
    HighLevelAPI.demo
    Sample.demo
