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

<https://github.com/rcook/hgeos/blob/master/src/test/GEOSTest/HighLevelAPI.hs View sample 1>

<https://github.com/rcook/hgeos/blob/master/src/test/GEOSTest/Sample.hs View sample 2>
-}

{-# LANGUAGE RecordWildCards #-}

module Data.Geolocation.GEOS
    ( Context ()
    , CoordinateSequence ()
    , Geometry ()
    , GeometryType (..)
    , Reader ()
    , Writer ()
    , area
    , coordinateSequence
    , envelope
    , exteriorRing
    , geometryType
    , getGeometry
    , getNumGeometries
    , getSize
    , getX
    , getY
    , getZ
    , intersection
    , isEmpty
    , mkReader
    , mkWriter
    , readGeometry
    , version
    , withGEOS
    , writeGeometry
    ) where

import Control.Exception
import Data.Geolocation.GEOS.Imports
import Data.IORef
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

-- |Represents a <https://trac.osgeo.org/geos/ GEOS> context
data Context = Context ContextStateRef

data ContextState = ContextState
    { hCtx :: GEOSContextHandle_t
    , hReaders :: [GEOSWKTReaderPtr]
    , hWriters :: [GEOSWKTWriterPtr]
    , hGeometries :: [GEOSGeometryPtr]
    , hCoordinateSequences :: [GEOSCoordSequencePtr]
    }

type ContextStateRef = IORef ContextState

-- |References a <https://trac.osgeo.org/geos/ GEOS> coordinate sequence
data CoordinateSequence = CoordinateSequence ContextStateRef GEOSCoordSequencePtr

-- |Represents a <https://trac.osgeo.org/geos/ GEOS> geometry type ID
data GeometryType =
    Point |
    LineString |
    LinearRing |
    Polygon |
    MultiPoint |
    MultlLineString |
    MultiPolygon |
    GeometryCollection deriving (Enum, Show)

-- |References a <https://en.wikipedia.org/wiki/Well-known_text WKT> reader
data Reader = Reader ContextStateRef GEOSWKTReaderPtr

-- |References a <https://en.wikipedia.org/wiki/Well-known_text WKT> writer
data Writer = Writer ContextStateRef GEOSWKTWriterPtr

-- |References a <https://trac.osgeo.org/geos/ GEOS> geometry
data Geometry = Geometry ContextStateRef GEOSGeometryPtr

-- |Returns area of a 'Geometry' instance
area :: Geometry -> IO (Maybe Double)
area (Geometry sr h) = do
    ContextState{..} <- readIORef sr
    alloca $ \valuePtr -> do
        status <- c_GEOSArea_r hCtx h valuePtr
        case status of
             0 -> return Nothing
             _ -> do
                value <- peek valuePtr
                return $ Just (realToFrac value)

checkAndDoNotTrack :: ContextStateRef -> (GEOSContextHandle_t -> IO GEOSGeometryPtr) -> IO (Maybe Geometry)
checkAndDoNotTrack sr f = do
    ContextState{..} <- readIORef sr
    h <- f hCtx
    return $ if isNullPtr h
                then Nothing
                else Just $ Geometry sr h

checkAndTrack :: ContextStateRef -> (GEOSContextHandle_t -> IO GEOSGeometryPtr) -> IO (Maybe Geometry)
checkAndTrack sr f = do
    ContextState{..} <- readIORef sr
    h <- f hCtx
    if isNullPtr h
    then return Nothing
    else do
        modifyIORef' sr $ (\p@ContextState{..} -> p { hGeometries = h : hGeometries })
        return $ Just (Geometry sr h)

-- |Returns a 'CoordinateSequence' from the supplied 'Geometry'
coordinateSequence :: Geometry -> IO (Maybe CoordinateSequence)
coordinateSequence (Geometry sr hGeometry) = do
    ContextState{..} <- readIORef sr
    h <- c_GEOSGeom_getCoordSeq_r hCtx hGeometry
    return $ if isNullPtr h
                then Nothing
                else Just $ CoordinateSequence sr h

-- |Returns a 'Geometry' instance representing the envelope of the supplied
-- 'Geometry'
envelope :: Geometry -> IO (Maybe Geometry)
envelope (Geometry sr h) =
    checkAndTrack sr (\hCtx -> c_GEOSEnvelope_r hCtx h)

-- |Returns a 'Geometry' instance representing the exterior ring of the
-- supplied 'Geometry'
exteriorRing :: Geometry -> IO (Maybe Geometry)
exteriorRing (Geometry sr h) =
    checkAndDoNotTrack sr (\hCtx -> c_GEOSGetExteriorRing_r hCtx h)

-- |Returns type of a 'Geometry' instance
geometryType :: Geometry -> IO (Maybe GeometryType)
geometryType (Geometry sr h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSGeomTypeId_r hCtx h
    return $ if value == -1
                then Nothing
                else Just $ toEnum (fromIntegral value)

-- |Returns child 'Geometry' at given index
getGeometry :: Geometry -> Int -> IO (Maybe Geometry)
getGeometry (Geometry sr h) index =
    checkAndDoNotTrack sr (\hCtx -> c_GEOSGetGeometryN_r hCtx h (fromIntegral index))

-- |Gets the number of geometries in a 'Geometry' instance
getNumGeometries :: Geometry -> IO (Maybe Int)
getNumGeometries (Geometry sr h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSGetNumGeometries_r hCtx h
    return $ if value == -1
                then Nothing
                else Just $ fromIntegral value

getOrdinate :: (GEOSContextHandle_t -> GEOSCoordSequencePtr -> CUInt -> Ptr CDouble -> IO CInt) ->
    CoordinateSequence -> Word -> IO (Maybe Double)
getOrdinate f (CoordinateSequence sr h) index = do
    ContextState{..} <- readIORef sr
    alloca $ \valuePtr -> do
        status <- f hCtx h (fromIntegral index) valuePtr
        case status of
             0 -> return Nothing
             _ -> do
                value <- peek valuePtr
                return $ Just (realToFrac value)

-- |Gets the size from a coordinate sequence
getSize :: CoordinateSequence -> IO (Maybe Word)
getSize (CoordinateSequence sr h) = do
    ContextState{..} <- readIORef sr
    alloca $ \sizePtr -> do
        status <- c_GEOSCoordSeq_getSize_r hCtx h sizePtr
        case status of
             0 -> return Nothing
             _ -> do
                 size <- peek sizePtr
                 return $ Just (fromIntegral size)

-- |Gets an "x" ordinate value from a coordinate sequence
getX :: CoordinateSequence -> Word -> IO (Maybe Double)
getX = getOrdinate c_GEOSCoordSeq_getX_r

-- |Gets a "y" ordinate value from a coordinate sequence
getY :: CoordinateSequence -> Word -> IO (Maybe Double)
getY = getOrdinate c_GEOSCoordSeq_getY_r

-- |Gets a "z" ordinate value from a coordinate sequence
getZ :: CoordinateSequence -> Word -> IO (Maybe Double)
getZ = getOrdinate c_GEOSCoordSeq_getZ_r

-- |Returns a 'Geometry' instance representing the intersection of the two
-- supplied 'Geometry' instances:
intersection :: Geometry -> Geometry -> IO (Maybe Geometry)
intersection (Geometry sr0 h0) (Geometry sr1 h1) =
    checkAndTrack sr0 (\hCtx -> c_GEOSIntersection_r hCtx h0 h1)

-- |Returns value indicating if specified 'Geometry' instance is empty
isEmpty :: Geometry -> IO Bool
isEmpty (Geometry sr h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSisEmpty_r hCtx h
    return $ value /= 0

mkContext :: IO Context
mkContext = do
    hCtx <- c_initializeGEOSWithHandlers
    sr <- newIORef $ ContextState hCtx [] [] [] []
    return $ Context sr

-- |Creates a reader used to deserialize 'Geometry' instances from
-- <https://en.wikipedia.org/wiki/Well-known_text WKT>-format text:
mkReader :: Context -> IO Reader
mkReader (Context sr) = do
    ContextState{..} <- readIORef sr
    h <- c_GEOSWKTReader_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hReaders = h : hReaders })
    return $ Reader sr h

-- |Creates a writer used to serialize 'Geometry' instances to
-- <https://en.wikipedia.org/wiki/Well-known_text WKT>-format text:
mkWriter :: Context -> IO Writer
mkWriter (Context sr) = do
    ContextState{..} <- readIORef sr
    h <- c_GEOSWKTWriter_create_r hCtx
    modifyIORef' sr (\p@ContextState{..} -> p { hWriters = h : hWriters })
    return $ Writer sr h

-- |Deserializes a 'Geometry' instance from the given 'String' using the
-- supplied 'Reader':
readGeometry :: Reader -> String -> IO (Maybe Geometry)
readGeometry (Reader sr h) str = withCString str $ \cs -> do
    checkAndTrack sr (\hCtx -> c_GEOSWKTReader_read_r hCtx h cs)

releaseContext :: Context -> IO ()
releaseContext (Context sr) = do
    ContextState{..} <- readIORef sr
    mapM_ (c_GEOSCoordSeq_destroy_r hCtx) hCoordinateSequences
    mapM_ (c_GEOSGeom_destroy_r hCtx) hGeometries
    mapM_ (c_GEOSWKTWriter_destroy_r hCtx) hWriters
    mapM_ (c_GEOSWKTReader_destroy_r hCtx) hReaders
    c_finishGEOS_r hCtx

-- |Reports version of GEOS API
version :: IO String
version = c_GEOSversion >>= peekCString

-- |Creates a <https://trac.osgeo.org/geos/ GEOS> context, passes it to a block
-- and releases the context and all associated objects such as readers, writers
-- and geometries at the end:
--
-- @
--    withGEOS $ \ctx -> do
--
--        -- Use context
--
--        return ()
-- @
withGEOS :: (Context -> IO a) -> IO a
withGEOS = bracket mkContext releaseContext

-- |Serializes a 'Geometry' instance to a 'String' using the supplied 'Writer':
writeGeometry :: Writer -> Geometry -> IO String
writeGeometry (Writer sr hWriter) (Geometry _ hGeometry) = do
    ContextState{..} <- readIORef sr
    str <- bracket
        (c_GEOSWKTWriter_write_r hCtx hWriter hGeometry)
        (c_GEOSFree_r_CString hCtx)
        peekCString
    return str
