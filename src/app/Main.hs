module Main (main) where

import Control.Exception
import Data.Geolocation.GEOS
import Data.Geolocation.GEOS.Imports
import Foreign.C
import Foreign.Ptr
import Paths_hgeos

-- Demonstrates direct use of imports
-- Lifetimes of various GEOS objects, including readers, writers and
-- geometries, must be managed explicitly by clients using explicit calls to
-- various destroy/free functions
lowLevelAPIDemo :: IO ()
lowLevelAPIDemo = do
    wkt0 <- newCString "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
    wkt1 <- newCString "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"

    withGEOS $ \ctx -> do
        withWKTReader ctx $ \reader -> do
            withGeometry ctx reader wkt0 $ \g0 -> do
                withGeometry ctx reader wkt1 $ \g1 -> do
                    bracket (c_GEOSIntersection_r ctx g0 g1) (c_GEOSGeom_destroy_r ctx) $ \g2 -> do
                        withWKTWriter ctx $ \writer -> do
                            str <- bracket
                                (c_GEOSWKTWriter_write_r ctx writer g2)
                                (c_GEOSFree_r_CString ctx)
                                peekCString
                            putStrLn str
                            putStrLn "lowLevelAPIDemo done"
    where
        withGEOS :: (GEOSContextHandle_t -> IO a) -> IO a
        withGEOS = bracket c_initializeGEOSWithHandlers c_uninitializeGEOS
        withWKTReader :: GEOSContextHandle_t -> (GEOSWKTReaderPtr -> IO a) -> IO a
        withWKTReader ctx = bracket (c_GEOSWKTReader_create_r ctx) (c_GEOSWKTReader_destroy_r ctx)
        withWKTWriter :: GEOSContextHandle_t -> (GEOSWKTWriterPtr -> IO a) -> IO a
        withWKTWriter ctx = bracket (c_GEOSWKTWriter_create_r ctx) (c_GEOSWKTWriter_destroy_r ctx)
        withGeometry :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> CString -> (GEOSGeometryPtr -> IO a) -> IO a
        withGeometry ctx reader wkt =
            bracket
                (c_GEOSWKTReader_read_r ctx reader wkt)
                (c_GEOSGeom_destroy_r ctx)

-- Demonstrates use of high-level API
-- Lifetimes of GEOS objects are automatically managed by the context objects
-- which guarantees that they are released when the context goes out of scope
highLevelAPIDemo :: IO ()
highLevelAPIDemo = do
    withContext $ \ctx -> do
        reader <- mkReader ctx
        g0 <- readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- readGeometry reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- intersection g0 g1
        writer <- mkWriter ctx
        str <- writeGeometry writer g2
        putStrLn str
        putStrLn "highLevelAPIDemo done"

main :: IO ()
main = do
    --s <- peekCString c_GEOSversion
    --putStrLn s
    --lowLevelAPIDemo
    --highLevelAPIDemo
    namibiaDemo

printGeometry :: Writer -> Geometry -> IO ()
printGeometry r g = writeGeometry r g >>= putStrLn

namibiaDemo :: IO ()
namibiaDemo = do
    fileName <- getDataFileName "data/namibia.wkt"
    wkt <- readFile fileName
    withContext $ \ctx -> do
        reader <- mkReader ctx
        writer <- mkWriter ctx
        let p = printGeometry writer

        country <- readGeometry reader wkt
        env <- envelope country
        shell <- exteriorRing env
        coordSeq <- coordinateSequence shell
        p shell
        return ()
