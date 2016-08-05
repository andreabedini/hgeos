module GEOSTest.TransAPI (demo) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Geolocation.GEOS.Trans
import Data.Maybe

check :: Bool -> MaybeT IO ()
check True = return ()
check False = error "Check failed"

-- Demonstrates use of monad transformer API
-- Lifetimes of GEOS objects are automatically managed by the context objects
-- which guarantees that they are released when the context goes out of scope
demo :: IO ()
demo = do
    result <- runGEOSEither $ \ctx -> do
        reader <- mkReaderM ctx
        writer <- mkWriterM ctx

        g0 <- readGeometryM reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- readGeometryM reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- intersectionM g0 g1
        str0 <- writeGeometryM writer g2
        lift $ putStrLn str0

        coords <- createCoordSeqM ctx 10 3
        size <- getSizeM coords
        forM_ [0..size - 2] $ \i -> do
            setXM coords i (fromIntegral (i + 1) * 10.0)
            setYM coords i (fromIntegral (i + 1) * 20.0)
            setZM coords i (fromIntegral (i + 1) * 30.0)
        setOrdinateM coords (size - 1) 0 10.0
        setOrdinateM coords (size - 1) 1 20.0
        setOrdinateM coords (size - 1) 2 30.0
        g3 <- createLinearRingM coords
        str1 <- writeGeometryM writer g3
        lift $ putStrLn str1

        x0 <- getXM coords 0
        check $ x0 == 10.0
        y0 <- getYM coords 0
        check $ y0 == 20.0
        z0 <- getZM coords 0
        check $ z0 == 30.0

        x1 <- getOrdinateM coords 1 0
        check $ x1 == 20.0
        y1 <- getOrdinateM coords 1 1
        check $ y1 == 40.0
        z1 <- getOrdinateM coords 1 2
        check $ z1 == 60.0

        p <- createPolygonM g3 []
        str2 <- writeGeometryM writer p
        lift $ putStrLn str2

    case result of
         Left m -> error $ "TransAPI.demo failed: " ++ m
         Right _ -> putStrLn "TransAPI.demo succeeded"
