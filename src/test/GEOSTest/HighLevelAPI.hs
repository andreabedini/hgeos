module GEOSTest.HighLevelAPI (demo) where

import Data.Geolocation.GEOS

-- Demonstrates use of high-level API
-- Lifetimes of GEOS objects are automatically managed by the context objects
-- which guarantees that they are released when the context goes out of scope
demo :: IO ()
demo = do
    withGEOS $ \ctx -> do
        reader <- mkReader ctx
        g0 <- readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- readGeometry reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- intersection g0 g1
        writer <- mkWriter ctx
        str <- writeGeometry writer g2
        putStrLn str
        putStrLn "HighLevelAPI.demo done"
