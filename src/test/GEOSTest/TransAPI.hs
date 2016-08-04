module GEOSTest.TransAPI (demo) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Geolocation.GEOS.Trans
import Data.Maybe

-- Demonstrates use of monad transformer API
-- Lifetimes of GEOS objects are automatically managed by the context objects
-- which guarantees that they are released when the context goes out of scope
demo :: IO ()
demo = do
    result <- runGEOS $ \ctx -> do
        reader <- mkReaderM ctx
        writer <- mkWriterM ctx

        g0 <- readGeometryM reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- readGeometryM reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- intersectionM g0 g1
        str0 <- writeGeometryM writer g2
        lift $ putStrLn str0

        coords <- createCoordSeqM ctx 10 3
        size <- getSizeM coords
        forM_ [0..size - 1] $ \i -> do
            setXM coords i (fromIntegral i * 10.0)
            setYM coords i (fromIntegral i * 20.0)
            setZM coords i (fromIntegral i * 30.0)
        g3 <- createLinearRingM coords
        str1 <- writeGeometryM writer g3
        lift $ putStrLn str1

    putStrLn $ "TransAPI.demo: " ++ (if isJust result then "succeeded" else "failed")
