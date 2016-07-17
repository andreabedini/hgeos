{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Foreign.C
import Data.Geocoding.GEOS.HighLevelAPI
import Data.Geocoding.GEOS.LowLevelAPI

lowLevelAPIDemo :: IO ()
lowLevelAPIDemo = do
    wkt0 <- newCString "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
    wkt1 <- newCString "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"

    withGEOS $ \h -> do
        (g0, g1) <- withWKTReader h $ \reader -> do
            -- Bangs required so reads occur before context is destroyed
            !(Just g0) <- wrap <$> c_GEOSWKTReader_read_r h reader wkt0
            !(Just g1) <- wrap <$> c_GEOSWKTReader_read_r h reader wkt1
            return (g0, g1)
        !(Just g2) <- wrap <$> c_GEOSIntersection_r h g0 g1
        withWKTWriter h $ \writer -> do
            g2cs <- c_GEOSWKTWriter_write_r h writer g2
            g2Str <- peekCString g2cs
            print g2Str
            c_GEOSFree_r_CChar h g2cs -- TODO: Use bracket
        c_GEOSGeom_destroy_r h g2 -- TODO: Use bracket
        c_GEOSGeom_destroy_r h g1 -- TODO: Use bracket
        c_GEOSGeom_destroy_r h g0 -- TODO: Use bracket
        putStrLn "lowLevelAPIDemo done"

highLevelAPIDemo :: IO ()
highLevelAPIDemo = do
    withContext $ \ctx -> do
        r <- mkReader ctx
        g0 <- readGeometry r "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- readGeometry r "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- intersection g0 g1
        w <- mkWriter ctx
        s2 <- writeGeometry w g2
        print s2
        putStrLn "highLevelAPIDemo done"

main :: IO ()
main = do
    s <- peekCString c_GEOSversion
    putStrLn s
    lowLevelAPIDemo
    highLevelAPIDemo
