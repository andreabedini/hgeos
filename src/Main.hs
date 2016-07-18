{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Data.IORef
import Foreign.C
import Data.Geocoding.GEOS.HighLevelAPI
import Data.Geocoding.GEOS.LowLevelAPI

newtype Handle = Handle Int

data ContextState = ContextState
    { hCtx :: GEOSContextHandle_t
    , hReaders :: [GEOSWKTReaderPtr]
    , hWriters :: [GEOSWKTWriterPtr]
    , hGeometries :: [GEOSGeometryPtr]
    }

type ContextStateRef = IORef ContextState

data Context = Context ContextStateRef

data Reader = Reader ContextStateRef GEOSWKTReaderPtr

data Writer = Writer ContextStateRef GEOSWKTWriterPtr

data Geometry = Geometry ContextStateRef GEOSGeometryPtr

mkContext :: IO Context
mkContext = do
    hCtx <- c_initializeGEOSWithHandlers
    sr <- newIORef $ ContextState hCtx [] [] []
    return $ Context sr

releaseContext :: Context -> IO ()
releaseContext (Context sr) = do
    ContextState{..} <- readIORef sr
    mapM_ (c_GEOSGeom_destroy_r hCtx) hGeometries
    mapM_ (c_GEOSWKTWriter_destroy_r hCtx) hWriters
    mapM_ (c_GEOSWKTReader_destroy_r hCtx) hReaders
    c_uninitializeGEOS hCtx

mkReader__ :: Context -> IO Reader
mkReader__ (Context sr) = do
    ContextState{..} <- readIORef sr
    hReader <- c_GEOSWKTReader_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hReaders = hReader : hReaders })
    return $ Reader sr hReader

mkWriter__ :: Context -> IO Writer
mkWriter__ (Context sr) = do
    ContextState{..} <- readIORef sr
    hWriter <- c_GEOSWKTWriter_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hWriters = hWriter : hWriters })
    return $ Writer sr hWriter

readGeometry__ :: Reader -> String -> IO Geometry
readGeometry__ (Reader sr hReader) str = withCString str $ \cs -> do
    ContextState{..} <- readIORef sr
    hGeometry <- c_GEOSWKTReader_read_r hCtx hReader cs
    modifyIORef' sr (\p@ContextState{..} -> p { hGeometries = hGeometry : hGeometries })
    return $ Geometry sr hGeometry

writeGeometry__ :: Writer -> Geometry -> IO String
writeGeometry__ (Writer sr hWriter) (Geometry _ hGeometry) = do
    ContextState{..} <- readIORef sr
    cs <- c_GEOSWKTWriter_write_r hCtx hWriter hGeometry
    str <- peekCString cs
    c_GEOSFree_r_CChar hCtx cs
    return str

intersection__ :: Geometry -> Geometry -> IO Geometry
intersection__ (Geometry sr0 hGeometry0) (Geometry sr1 hGeometry1) = do
    ContextState{..} <- readIORef sr0
    hGeometry <- c_GEOSIntersection_r hCtx hGeometry0 hGeometry1
    return $ Geometry sr0 hGeometry

withContext__ :: (Context -> IO a) -> IO a
withContext__ = bracket mkContext releaseContext

newAPIDemo :: IO ()
newAPIDemo = do
    withContext__ $ \ctx -> do
        reader <- mkReader__ ctx
        g0 <- readGeometry__ reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- readGeometry__ reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- intersection__ g0 g1
        writer <- mkWriter__ ctx
        str <- writeGeometry__ writer g2
        putStrLn str
        putStrLn "Done"

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
        g3 <- envelope g2

        w <- mkWriter ctx
        s3 <- writeGeometry w g3
        print s3

        putStrLn "highLevelAPIDemo done"

main :: IO ()
main = do
    s <- peekCString c_GEOSversion
    putStrLn s
    lowLevelAPIDemo
    highLevelAPIDemo
    newAPIDemo
