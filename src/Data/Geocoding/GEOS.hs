{-# LANGUAGE RecordWildCards #-}

module Data.Geocoding.GEOS
    ( intersection
    , mkReader
    , mkWriter
    , readGeometry
    , withContext
    , writeGeometry
    ) where

import Control.Exception
import Data.Geocoding.GEOS.LowLevelImports
import Data.IORef
import Foreign.C

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

withContext :: (Context -> IO a) -> IO a
withContext = bracket mkContext releaseContext

mkReader :: Context -> IO Reader
mkReader (Context sr) = do
    ContextState{..} <- readIORef sr
    hReader <- c_GEOSWKTReader_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hReaders = hReader : hReaders })
    return $ Reader sr hReader

mkWriter :: Context -> IO Writer
mkWriter (Context sr) = do
    ContextState{..} <- readIORef sr
    hWriter <- c_GEOSWKTWriter_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hWriters = hWriter : hWriters })
    return $ Writer sr hWriter

readGeometry :: Reader -> String -> IO Geometry
readGeometry (Reader sr hReader) str = withCString str $ \cs -> do
    ContextState{..} <- readIORef sr
    hGeometry <- c_GEOSWKTReader_read_r hCtx hReader cs
    modifyIORef' sr (\p@ContextState{..} -> p { hGeometries = hGeometry : hGeometries })
    return $ Geometry sr hGeometry

writeGeometry :: Writer -> Geometry -> IO String
writeGeometry (Writer sr hWriter) (Geometry _ hGeometry) = do
    ContextState{..} <- readIORef sr
    str <- bracket
        (c_GEOSWKTWriter_write_r hCtx hWriter hGeometry)
        (c_GEOSFree_r_CChar hCtx)
        peekCString
    return str

intersection :: Geometry -> Geometry -> IO Geometry
intersection (Geometry sr0 hGeometry0) (Geometry sr1 hGeometry1) = do
    ContextState{..} <- readIORef sr0
    hGeometry <- c_GEOSIntersection_r hCtx hGeometry0 hGeometry1
    return $ Geometry sr0 hGeometry
