module GEOSTest.HighLevelAPI (demo) where

import Control.Monad
import Data.Geolocation.GEOS
import Data.Maybe

check :: Bool -> IO ()
check True = return ()
check False = error "Check failed"

type Coordinate = (Double, Double)

createLinearRingHelper :: Context -> [Coordinate] -> IO Geometry
createLinearRingHelper ctx cs = do
    let count = length cs
    coords <- createCoordSeq ctx (fromIntegral count) 2
    forM_ (zip [0..] cs) $ \(i, (x, y)) -> do
        setX coords i x
        setY coords i y
    createLinearRing coords

-- Demonstrates use of high-level API
-- Lifetimes of GEOS objects are automatically managed by the context objects
-- which guarantees that they are released when the context goes out of scope
demo :: IO ()
demo = do
    withGEOS $ \ctx -> do
        reader <- mkReader ctx
        writer <- mkWriter ctx

        g0 <- readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        a0 <- area g0
        putStrLn $ show a0

        g1 <- readGeometry reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        a1 <- area g1
        putStrLn $ show a1

        g2 <- intersection g0 g1
        str0 <- writeGeometry writer g2
        putStrLn str0

        coords <- createCoordSeq ctx 10 3
        size <- getSize coords
        forM_ [0..size - 2] $ \i -> do
            setX coords i (fromIntegral (i + 1) * 10.0)
            setY coords i (fromIntegral (i + 1) * 20.0)
            setZ coords i (fromIntegral (i + 1) * 30.0)
        setOrdinate coords (size - 1) 0 10.0
        setOrdinate coords (size - 1) 1 20.0
        setOrdinate coords (size - 1) 2 30.0
        g3 <- createLinearRing coords
        str1 <- writeGeometry writer g3
        putStrLn str1

        x0 <- getX coords 0
        check $ x0 == 10.0
        y0 <- getY coords 0
        check $ y0 == 20.0
        z0 <- getZ coords 0
        check $ z0 == 30.0

        x1 <- getOrdinate coords 1 0
        check $ x1 == 20.0
        y1 <- getOrdinate coords 1 1
        check $ y1 == 40.0
        z1 <- getOrdinate coords 1 2
        check $ z1 == 60.0

        p <- createPolygon g3 []
        str2 <- writeGeometry writer p
        putStrLn str2

        shell <- createLinearRingHelper ctx [(-10.0, -10.0), (-10.0, 10.0), (10.0, 10.0), (10.0, -10.0), (-10.0, -10.0)]
        hole <- createLinearRingHelper ctx [(-5.0, -5.0), (-5.0, 5.0), (5.0, 5.0), (5.0, -5.0), (-5.0, -5.0)]
        polygon <- createPolygon shell [hole]
        str3 <- writeGeometry writer polygon
        putStrLn str3

        coll <- createCollection multiPolygon [polygon]
        str4 <- writeGeometry writer coll
        putStrLn str4

        p <- createEmptyPolygon ctx
        str5 <- writeGeometry writer p
        putStrLn str5

    putStrLn "HighLevelAPI2.demo succeeded"
