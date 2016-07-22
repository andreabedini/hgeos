{-|
Module      : Data.Geolocation.GEOS
Description : High-level API for interoperating with GEOS C API
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : POSIX

A high-level API for interoperating with GEOS C API which includes automatic
management of lifetimes of objects such as readers, writers and geometries.

For the low-level FFI bindings, see "Data.Geolocation.GEOS.Imports".
-}

{-# LANGUAGE RecordWildCards #-}

module Data.Geolocation.GEOS
    ( Context ()
    , Geometry ()
    , Reader ()
    , Writer ()
    , envelope
    , exteriorRing
    , intersection
    , mkReader
    , mkWriter
    , readGeometry
    , withContext
    , writeGeometry
    ) where

import Control.Exception
import Data.Geolocation.GEOS.Imports
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

track :: ContextStateRef -> (GEOSContextHandle_t -> IO GEOSGeometryPtr) -> IO Geometry
track sr f = do
    ContextState{..} <- readIORef sr
    h <- f hCtx
    modifyIORef' sr $ (\p@ContextState{..} -> p { hGeometries = h : hGeometries })
    return $ Geometry sr h

doNotTrack :: ContextStateRef -> (GEOSContextHandle_t -> IO GEOSGeometryPtr) -> IO Geometry
doNotTrack sr f = do
    ContextState{..} <- readIORef sr
    h <- f hCtx
    return $ Geometry sr h

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
    h <- c_GEOSWKTReader_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hReaders = h : hReaders })
    return $ Reader sr h

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
    h <- c_GEOSWKTWriter_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hWriters = h : hWriters })
    return $ Writer sr h

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
readGeometry (Reader sr h) str = withCString str $ \cs -> do
    track sr (\hCtx -> c_GEOSWKTReader_read_r hCtx h cs)

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
        (c_GEOSFree_r_CString hCtx)
        peekCString
    return str

-- |Returns the 'Geometry' instance representing the envelope of the supplied
-- 'Geometry' instance:
--
-- @
--    withContext $ \ctx -> do
--        reader <- mkReader ctx
--        g0 <- readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
--
--        g1 <- envelope g0
--
--        -- Use geometry
-- @
--
-- The geometry is associated with the 'Context' and released when the 'Context'
-- is released.
envelope :: Geometry -> IO Geometry
envelope (Geometry sr h) =
    track sr (\hCtx -> c_GEOSEnvelope_r hCtx h)

-- |Returns the 'Geometry' instance representing the exterior ring of the
-- supplied 'Geometry' instance:
--
-- @
--    withContext $ \ctx -> do
--        reader <- mkReader ctx
--        g0 <- readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
--
--        g1 <- exteriorRing g0
--
--        -- Use geometry
-- @
--
-- The geometry is managed by GEOS internally and is, therefore, not tracked by
-- the 'Context'.
exteriorRing :: Geometry -> IO Geometry
exteriorRing (Geometry sr h) =
    doNotTrack sr (\hCtx -> c_GEOSGetExteriorRing_r hCtx h)

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
-- @
--
-- The geometry is associated with the 'Context' and released when the 'Context'
-- is released.
intersection :: Geometry -> Geometry -> IO Geometry
intersection (Geometry sr0 h0) (Geometry sr1 h1) =
    track sr0 (\hCtx -> c_GEOSIntersection_r hCtx h0 h1)
