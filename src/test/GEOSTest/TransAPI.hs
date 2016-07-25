module GEOSTest.TransAPI (demo) where

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
        g0 <- readGeometryM reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- readGeometryM reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- intersectionM g0 g1
        writer <- mkWriterM ctx
        str <- writeGeometryM writer g2
        lift $ putStrLn str
    putStrLn $ "TransAPI.demo: " ++ (if isJust result then "succeeded" else "failed")
