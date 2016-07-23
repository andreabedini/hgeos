module GEOSTest.HighLevelAPI (demo) where

import Data.Geolocation.GEOS

-- Demonstrates use of high-level API
-- Lifetimes of GEOS objects are automatically managed by the context objects
-- which guarantees that they are released when the context goes out of scope
demo :: IO ()
demo = do
    withGEOS $ \ctx -> do
        (Just reader) <- mkReader ctx
        (Just g0) <- readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        (Just g1) <- readGeometry reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        (Just g2) <- intersection g0 g1
        (Just writer) <- mkWriter ctx
        (Just str) <- writeGeometry writer g2
        putStrLn str
        putStrLn "HighLevelAPI.demo done"
