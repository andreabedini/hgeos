{-|
Module      : Data.Geolocation.GEOS
Description : High-level API for interoperating with GEOS C API
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

A high-level API for interoperating with GEOS C API which includes automatic
management of lifetimes of objects such as readers, writers and geometries.

For the low-level FFI bindings, see "Data.Geolocation.GEOS.Imports".

For the monad transformer wrappers, see "Data.Geolocation.GEOS.Trans".

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
    , createCoordSeq
    , createLinearRing
    , envelope
    , geomTypeId
    , getCoordSeq
    , getErrorMessage
    , getExteriorRing
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
    , setX
    , setY
    , setZ
    , version
    , withGEOS
    , writeGeometry
    ) where

import Control.Exception
import Control.Monad
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
    { hCtx :: GEOSContextHandle
    , deleteActions :: [DeleteAction]
    }

type ContextStateRef = IORef ContextState

-- |References a <https://trac.osgeo.org/geos/ GEOS> coordinate sequence
data CoordinateSequence = CoordinateSequence ContextStateRef DeleteAction GEOSCoordSequencePtr

data DeleteAction = DeleteAction IntPtr (IO ())

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
data Reader = Reader ContextStateRef DeleteAction GEOSWKTReaderPtr

-- |References a <https://en.wikipedia.org/wiki/Well-known_text WKT> writer
data Writer = Writer ContextStateRef DeleteAction GEOSWKTWriterPtr

-- |References a <https://trac.osgeo.org/geos/ GEOS> geometry
data Geometry = Geometry ContextStateRef DeleteAction GEOSGeometryPtr

-- |Returns area of a 'Geometry' instance
area :: Geometry -> IO (Maybe Double)
area (Geometry sr _ h) = do
    ContextState{..} <- readIORef sr
    alloca $ \valuePtr -> do
        status <- c_GEOSArea_r hCtx h valuePtr
        case status of
             0 -> return Nothing
             _ -> do
                value <- peek valuePtr
                return $ Just (realToFrac value)

checkAndDoNotTrack :: ContextStateRef -> (GEOSContextHandle -> IO GEOSGeometryPtr) -> IO (Maybe Geometry)
checkAndDoNotTrack sr f = do
    ContextState{..} <- readIORef sr
    h <- f hCtx
    return $ if isNullPtr h
                then Nothing
                else Just $ Geometry sr emptyDeleteAction h

checkAndTrack :: NullablePtr a =>
    ContextStateRef ->
    (GEOSContextHandle -> IO a) ->
    (GEOSContextHandle -> a -> IO ()) ->
    (ContextStateRef -> DeleteAction -> a -> b) ->
    IO (Maybe b)
checkAndTrack sr create destroy wrap = do
    ContextState{..} <- readIORef sr
    h <- create hCtx
    if isNullPtr h
    then return Nothing
    else do
        let deleteAction = DeleteAction (rawIntPtr h) (destroy hCtx h)
        modifyIORef' sr (\p@ContextState{..} -> p { deleteActions = deleteAction : deleteActions })
        return $ Just (wrap sr deleteAction h)

checkAndTrackGeometry :: ContextStateRef -> (GEOSContextHandle -> IO GEOSGeometryPtr) -> IO (Maybe Geometry)
checkAndTrackGeometry sr create = checkAndTrack sr create c_GEOSGeom_destroy_r Geometry

emptyDeleteAction :: DeleteAction
emptyDeleteAction = DeleteAction (ptrToIntPtr nullPtr) (return ())

-- |Creates an empty 'CoordinateSequence' instance
createCoordSeq :: Context -> Word -> Word -> IO (Maybe CoordinateSequence)
createCoordSeq (Context sr) size dims =
    checkAndTrack
        sr
        (\hCtx -> c_GEOSCoordSeq_create_r hCtx (fromIntegral size) (fromIntegral dims))
        c_GEOSCoordSeq_destroy_r
        CoordinateSequence

-- |Returns a linear ring 'Geometry' instance from the given coordinate
-- sequence
createLinearRing :: CoordinateSequence -> IO (Maybe Geometry)
createLinearRing (CoordinateSequence sr (DeleteAction rawPtr _) h) = do
    modifyIORef' sr $ \p@ContextState{..} -> p { deleteActions = filter (\(DeleteAction r _) -> r /= rawPtr) deleteActions }
    checkAndTrackGeometry sr (\hCtx -> c_GEOSGeom_createLinearRing_r hCtx h)

-- |Returns a 'Geometry' instance representing the envelope of the supplied
-- 'Geometry'
envelope :: Geometry -> IO (Maybe Geometry)
envelope (Geometry sr _ h) =
    checkAndTrackGeometry sr (\hCtx -> c_GEOSEnvelope_r hCtx h)

-- |Returns type of a 'Geometry' instance
geomTypeId :: Geometry -> IO (Maybe GeometryType)
geomTypeId (Geometry sr _ h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSGeomTypeId_r hCtx h
    return $ if value == -1
                then Nothing
                else Just $ toEnum (fromIntegral value)

-- |Returns a 'CoordinateSequence' from the supplied 'Geometry'
getCoordSeq :: Geometry -> IO (Maybe CoordinateSequence)
getCoordSeq (Geometry sr _ hGeometry) = do
    ContextState{..} <- readIORef sr
    h <- c_GEOSGeom_getCoordSeq_r hCtx hGeometry
    return $ if isNullPtr h
                then Nothing
                else Just $ CoordinateSequence sr emptyDeleteAction h

-- |Returns message in case of error
getErrorMessage :: IO String
getErrorMessage = c_getErrorMessage >>= peekCString

-- |Returns a 'Geometry' instance representing the exterior ring of the
-- supplied 'Geometry'
getExteriorRing :: Geometry -> IO (Maybe Geometry)
getExteriorRing (Geometry sr _ h) =
    checkAndDoNotTrack sr (\hCtx -> c_GEOSGetExteriorRing_r hCtx h)

-- |Returns child 'Geometry' at given index
getGeometry :: Geometry -> Int -> IO (Maybe Geometry)
getGeometry (Geometry sr _ h) index =
    checkAndDoNotTrack sr (\hCtx -> c_GEOSGetGeometryN_r hCtx h (fromIntegral index))

-- |Gets the number of geometries in a 'Geometry' instance
getNumGeometries :: Geometry -> IO (Maybe Int)
getNumGeometries (Geometry sr _ h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSGetNumGeometries_r hCtx h
    return $ if value == -1
                then Nothing
                else Just $ fromIntegral value

getOrdinate :: (GEOSContextHandle -> GEOSCoordSequencePtr -> CUInt -> Ptr CDouble -> IO CInt) ->
    CoordinateSequence -> Word -> IO (Maybe Double)
getOrdinate f (CoordinateSequence sr _ h) index = do
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
getSize (CoordinateSequence sr _ h) = do
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
intersection (Geometry sr0 _ h0) (Geometry sr1 _ h1) =
    checkAndTrackGeometry sr0 (\hCtx -> c_GEOSIntersection_r hCtx h0 h1)

-- |Returns value indicating if specified 'Geometry' instance is empty
isEmpty :: Geometry -> IO (Maybe Bool)
isEmpty (Geometry sr _ h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSisEmpty_r hCtx h
    return $ case value of
                  0 -> Just False
                  1 -> Just True
                  _ -> Nothing

mkContext :: IO Context
mkContext = do
    hCtx <- c_initializeGEOSWithHandlers
    sr <- newIORef $ ContextState hCtx []
    return $ Context sr

-- |Creates a reader used to deserialize 'Geometry' instances from
-- <https://en.wikipedia.org/wiki/Well-known_text WKT>-format text:
mkReader :: Context -> IO (Maybe Reader)
mkReader (Context sr) = checkAndTrack sr c_GEOSWKTReader_create_r c_GEOSWKTReader_destroy_r Reader

-- |Creates a writer used to serialize 'Geometry' instances to
-- <https://en.wikipedia.org/wiki/Well-known_text WKT>-format text:
mkWriter :: Context -> IO (Maybe Writer)
mkWriter (Context sr) = checkAndTrack sr c_GEOSWKTWriter_create_r c_GEOSWKTWriter_destroy_r Writer

-- |Deserializes a 'Geometry' instance from the given 'String' using the
-- supplied 'Reader':
readGeometry :: Reader -> String -> IO (Maybe Geometry)
readGeometry (Reader sr _ h) str = withCString str $ \cs -> do
    checkAndTrackGeometry sr (\hCtx -> c_GEOSWKTReader_read_r hCtx h cs)

releaseContext :: Context -> IO ()
releaseContext (Context sr) = do
    ContextState{..} <- readIORef sr
    mapM_ (\(DeleteAction _ f) -> f) deleteActions
    c_finishGEOS_r hCtx

setOrdinate :: (GEOSContextHandle -> GEOSCoordSequencePtr -> CUInt -> CDouble -> IO CInt ) -> CoordinateSequence -> Word -> Double -> IO (Maybe ())
setOrdinate f (CoordinateSequence sr _ h) idx val = do
    ContextState{..} <- readIORef sr
    status <- f hCtx h (fromIntegral idx) (realToFrac val)
    return $ case status of
                  0 -> Nothing
                  _ -> Just ()

-- |Sets an "x" ordinate value within a coordinate sequence
setX :: CoordinateSequence -> Word -> Double -> IO (Maybe ())
setX = setOrdinate c_GEOSCoordSeq_setX_r

-- |Sets an "y" ordinate value within a coordinate sequence
setY :: CoordinateSequence -> Word -> Double -> IO (Maybe ())
setY = setOrdinate c_GEOSCoordSeq_setY_r

-- |Sets an "z" ordinate value within a coordinate sequence
setZ :: CoordinateSequence -> Word -> Double -> IO (Maybe ())
setZ = setOrdinate c_GEOSCoordSeq_setZ_r

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
writeGeometry :: Writer -> Geometry -> IO (Maybe String)
writeGeometry (Writer sr _ hWriter) (Geometry _ _ hGeometry) = do
    ContextState{..} <- readIORef sr
    bracket
        (c_GEOSWKTWriter_write_r hCtx hWriter hGeometry)
        (c_GEOSFree_r_CString hCtx)
        (\cs -> if cs == nullPtr then return Nothing else Just <$> peekCString cs)
