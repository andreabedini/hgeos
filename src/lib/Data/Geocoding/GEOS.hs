{-|
Module      : Data.Geocoding.GEOS
Description : High-level API for interoperating with GEOS C API
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : POSIX

A high-level API for interoperating with GEOS C API which includes automatic
management of lifetimes of objects such as readers, writers and geometries.

For the low-level FFI bindings, see "Data.Gecoding.GEOS.Imports".
-}

{-# LANGUAGE RecordWildCards #-}

module Data.Geocoding.GEOS
    ( Context()
    , Geometry()
    , Reader()
    , Writer()
    , intersection
    , mkReader
    , mkWriter
    , readGeometry
    , withContext
    , writeGeometry
    ) where

import Control.Exception
import Data.Geocoding.GEOS.Imports
import Data.IORef
import Foreign.C

data ContextState = ContextState
    { hCtx :: GEOSContextHandle_t
    , hReaders :: [GEOSWKTReaderPtr]
    , hWriters :: [GEOSWKTWriterPtr]
    , hGeometries :: [GEOSGeometryPtr]
    }

type ContextStateRef = IORef ContextState

-- |Represents a <https://trac.osgeo.org/geos/ GEOS> context
data Context = Context ContextStateRef

-- |Represents a <https://en.wikipedia.org/wiki/Well-known_text WKT> reader
data Reader = Reader ContextStateRef GEOSWKTReaderPtr

-- |Represents a <https://en.wikipedia.org/wiki/Well-known_text WKT> writer
data Writer = Writer ContextStateRef GEOSWKTWriterPtr

-- |Represents a <https://trac.osgeo.org/geos/ GEOS> geometry
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

-- |Creates a <https://trac.osgeo.org/geos/ GEOS> context, passes it to a block
-- and releases the context and all associated objects such as readers, writers
-- and geometries at the end:
--
-- @
--    withContext $ \ctx -> do
--
--        -- Use context
--
--        return ()
-- @
withContext :: (Context -> IO a) -> IO a
withContext = bracket mkContext releaseContext

-- |Creates a reader used to deserialize 'Geometry' instances from
-- <https://en.wikipedia.org/wiki/Well-known_text WKT>-format text:
--
-- @
--    withContext $ \ctx -> do
--        reader <- mkReader ctx
--
--        -- Use reader
--
--        return ()
-- @
--
-- The reader is associated with the 'Context' and released when the 'Context'
-- is released.
mkReader :: Context -> IO Reader
mkReader (Context sr) = do
    ContextState{..} <- readIORef sr
    hReader <- c_GEOSWKTReader_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hReaders = hReader : hReaders })
    return $ Reader sr hReader

-- |Creates a writer used to serialize 'Geometry' instances to
-- <https://en.wikipedia.org/wiki/Well-known_text WKT>-format text:
--
-- @
--    withContext $ \ctx -> do
--        writer <- mkWriter ctx
--
--        -- Use writer
--
--        return ()
-- @
--
-- The writer is associated with the 'Context' and released when the 'Context'
-- is released.
mkWriter :: Context -> IO Writer
mkWriter (Context sr) = do
    ContextState{..} <- readIORef sr
    hWriter <- c_GEOSWKTWriter_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hWriters = hWriter : hWriters })
    return $ Writer sr hWriter

-- |Deserializes a 'Geometry' instance from the given 'String' using the
-- supplied 'Reader':
--
-- @
--    withContext $ \ctx -> do
--        reader <- mkReader ctx
--        geometry <- readGeometry read "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
--
--        -- Use geometry
--
--        return ()
-- @
--
-- The geometry is associated with the 'Context' and released when the 'Context'
-- is released.
readGeometry :: Reader -> String -> IO Geometry
readGeometry (Reader sr hReader) str = withCString str $ \cs -> do
    ContextState{..} <- readIORef sr
    hGeometry <- c_GEOSWKTReader_read_r hCtx hReader cs
    modifyIORef' sr (\p@ContextState{..} -> p { hGeometries = hGeometry : hGeometries })
    return $ Geometry sr hGeometry

-- |Serializes a 'Geometry' instance to a 'String' using the supplied 'Writer':
--
-- @
--    withContext $ \ctx -> do
--        reader <- mkReader ctx
--        geometry <- readGeometry read "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
--
--        writer <- mkWriter ctx
--        str <- writeGeometry writer geometry
--
--        -- Use string
--
--        return ()
-- @
--
-- The geometry is associated with the 'Context' and released when the 'Context'
-- is released.
writeGeometry :: Writer -> Geometry -> IO String
writeGeometry (Writer sr hWriter) (Geometry _ hGeometry) = do
    ContextState{..} <- readIORef sr
    str <- bracket
        (c_GEOSWKTWriter_write_r hCtx hWriter hGeometry)
        (c_GEOSFree_r_CChar hCtx)
        peekCString
    return str

-- |Returns the 'Geometry' instance representing the intersection of the two
-- supplied 'Geometry' instances:
--
-- @
--    withContext $ \ctx -> do
--        reader <- mkReader ctx
--        g0 <- readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
--        g1 <- readGeometry reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
--
--        g2 <- intersection g0 g1
--
--        -- Use geometry
--
--        putStrLn str
-- @
--
-- The geometry is associated with the 'Context' and released when the 'Context'
-- is released.
intersection :: Geometry -> Geometry -> IO Geometry
intersection (Geometry sr0 hGeometry0) (Geometry sr1 hGeometry1) = do
    ContextState{..} <- readIORef sr0
    hGeometry <- c_GEOSIntersection_r hCtx hGeometry0 hGeometry1
    return $ Geometry sr0 hGeometry
