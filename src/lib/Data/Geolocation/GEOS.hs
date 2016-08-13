{-|
Module      : Data.Geolocation.GEOS
Description : High-level API for interoperating with GEOS C API
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

A high-level API for interoperating with Geometry Engine Open Source C API
which includes automatic management of lifetimes of objects such as readers,
writers and geometries.

For the low-level FFI bindings, see "Data.Geolocation.GEOS.Imports".

For the monad transformer wrappers, see "Data.Geolocation.GEOS.Trans".

<https://github.com/rcook/hgeos/blob/master/src/test/GEOSTest/HighLevelAPI.hs View sample 1>

<https://github.com/rcook/hgeos/blob/master/src/test/GEOSTest/Sample.hs View sample 2>
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Geolocation.GEOS
    ( module Data.Geolocation.GEOS.Internal
    , Context ()
    , CoordinateSequence ()
    , GEOSError ()
    , Geometry ()
    , Reader ()
    , Writer ()
    , area
    , createCollection
    , createCoordSeq
    , createEmptyPolygon
    , createLinearRing
    , createPolygon
    , envelope
    , geomTypeId
    , getCoordSeq
    , getErrorMessage
    , getExteriorRing
    , getGeometry
    , getNumGeometries
    , getOrdinate
    , getSize
    , getX
    , getY
    , getZ
    , intersection
    , isEmpty
    , mkReader
    , mkWriter
    , readGeometry
    , setOrdinate
    , setX
    , setY
    , setZ
    , version
    , withGEOS
    , writeGeometry
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Geolocation.GEOS.Imports
import Data.Geolocation.GEOS.Internal
import Data.IORef
import Data.Typeable
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
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

-- |References a <https://en.wikipedia.org/wiki/Well-known_text WKT> reader
data Reader = Reader ContextStateRef DeleteAction GEOSWKTReaderPtr

-- |References a <https://en.wikipedia.org/wiki/Well-known_text WKT> writer
data Writer = Writer ContextStateRef DeleteAction GEOSWKTWriterPtr

-- |Encapsulates an exception thrown by GEOS library
data GEOSError = GEOSError
    { context :: String
    , message :: String
    } deriving (Show, Typeable)
instance Exception GEOSError

-- |References a <https://trac.osgeo.org/geos/ GEOS> geometry
data Geometry = Geometry
    { geometryStateRef :: ContextStateRef
    , geometryDeleteAction :: DeleteAction
    , geometryRawPtr :: GEOSGeometryPtr
    }

throwGEOSError :: String -> IO a
throwGEOSError context = do
    m <- c_getErrorMessage
    message <- peekCString m
    throw $ GEOSError context message

-- |Returns area of a 'Geometry' instance
area :: Geometry -> IO Double
area (Geometry sr _ h) = do
    ContextState{..} <- readIORef sr
    alloca $ \valuePtr -> do
        status <- c_GEOSArea_r hCtx h valuePtr
        case status of
             0 -> throwGEOSError "GEOSArea_r"
             _ -> do
                value <- peek valuePtr
                return $ realToFrac value

checkAndDoNotTrack :: String -> ContextStateRef -> (GEOSContextHandle -> IO GEOSGeometryPtr) -> IO Geometry
checkAndDoNotTrack context sr f = do
    ContextState{..} <- readIORef sr
    h <- f hCtx
    if isNullPtr h
       then throwGEOSError context
       else return $ Geometry sr emptyDeleteAction h

checkAndTrack :: NullablePtr a =>
    String ->
    ContextStateRef ->
    (GEOSContextHandle -> IO a) ->
    (GEOSContextHandle -> a -> IO ()) ->
    (ContextStateRef -> DeleteAction -> a -> b) ->
    IO b
checkAndTrack context sr create destroy wrap = do
    ContextState{..} <- readIORef sr
    h <- create hCtx
    if isNullPtr h
    then throwGEOSError context
    else do
        let deleteAction = DeleteAction (rawIntPtr h) (destroy hCtx h)
        modifyIORef' sr (\p@ContextState{..} -> p { deleteActions = deleteAction : deleteActions })
        return $ wrap sr deleteAction h

checkAndTrackGeometry :: String -> ContextStateRef -> (GEOSContextHandle -> IO GEOSGeometryPtr) -> IO Geometry
checkAndTrackGeometry context sr create = checkAndTrack context sr create c_GEOSGeom_destroy_r Geometry

emptyDeleteAction :: DeleteAction
emptyDeleteAction = DeleteAction (ptrToIntPtr nullPtr) (return ())

-- |Creates a 'Geometry' collection
createCollection :: GeometryType -> [Geometry] -> IO Geometry
createCollection geometryType gs@((Geometry sr _ _) : _) = do
    untrack sr (map geometryDeleteAction gs)
    withArrayLen (map geometryRawPtr gs) $ \count array ->
        checkAndTrackGeometry
            "GEOSGeom_createCollection_r"
            sr
            (\hCtx -> c_GEOSGeom_createCollection_r hCtx (unGeometryType geometryType) array (fromIntegral count))

-- |Creates an empty 'CoordinateSequence' instance
createCoordSeq :: Context -> Word -> Word -> IO CoordinateSequence
createCoordSeq (Context sr) size dims =
    checkAndTrack
        "GEOSCoordSeq_create_r"
        sr
        (\hCtx -> c_GEOSCoordSeq_create_r hCtx (fromIntegral size) (fromIntegral dims))
        c_GEOSCoordSeq_destroy_r
        CoordinateSequence

-- |Returns an empty polygon 'Geometry' instance
createEmptyPolygon :: Context -> IO Geometry
createEmptyPolygon (Context sr) = do
    ContextState{..} <- readIORef sr
    checkAndTrackGeometry
        "GEOSGeom_createEmptyPolygon_r"
        sr
        c_GEOSGeom_createEmptyPolygon_r

-- |Returns a linear ring 'Geometry' instance from the given coordinate
-- sequence
createLinearRing :: CoordinateSequence -> IO Geometry
createLinearRing (CoordinateSequence sr deleteAction h) = do
    untrack sr [deleteAction]
    checkAndTrackGeometry
        "GEOSGeom_createLinearRing_r"
        sr
        (\hCtx -> c_GEOSGeom_createLinearRing_r hCtx h)

-- |Returns a polygon 'Geometry' instance from the given shell and optional
-- array of holes
createPolygon :: Geometry -> [Geometry] -> IO Geometry
createPolygon (Geometry sr deleteAction h) holes = do
    untrack sr (deleteAction : map geometryDeleteAction holes)
    withArrayLen (map geometryRawPtr holes) $ \count array ->
        checkAndTrackGeometry
            "GEOSGeom_createPolygon_r"
            sr
            (\hCtx -> c_GEOSGeom_createPolygon_r hCtx h array (fromIntegral count))

-- |Returns a 'Geometry' instance representing the envelope of the supplied
-- 'Geometry'
envelope :: Geometry -> IO Geometry
envelope (Geometry sr _ h) =
    checkAndTrackGeometry
        "GEOSEnvelope_r"
        sr
        (\hCtx -> c_GEOSEnvelope_r hCtx h)

-- |Returns type of a 'Geometry' instance
geomTypeId :: Geometry -> IO GeometryType
geomTypeId (Geometry sr _ h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSGeomTypeId_r hCtx h
    if value == -1
       then throwGEOSError "GEOSGeomTypeId_r"
       else return $ GeometryType value

-- |Returns a 'CoordinateSequence' from the supplied 'Geometry'
getCoordSeq :: Geometry -> IO CoordinateSequence
getCoordSeq (Geometry sr _ hGeometry) = do
    ContextState{..} <- readIORef sr
    h <- c_GEOSGeom_getCoordSeq_r hCtx hGeometry
    if isNullPtr h
       then throwGEOSError "GEOSGeom_getCoordSeq_r"
       else return $ CoordinateSequence sr emptyDeleteAction h

-- |Returns message in case of error
getErrorMessage :: IO String
getErrorMessage = c_getErrorMessage >>= peekCString

-- |Returns a 'Geometry' instance representing the exterior ring of the
-- supplied 'Geometry'
getExteriorRing :: Geometry -> IO Geometry
getExteriorRing (Geometry sr _ h) =
    checkAndDoNotTrack
        "GEOSGetExteriorRing_r"
        sr
        (\hCtx -> c_GEOSGetExteriorRing_r hCtx h)

-- |Returns child 'Geometry' at given index
getGeometry :: Geometry -> Int -> IO Geometry
getGeometry (Geometry sr _ h) n =
    checkAndDoNotTrack
        "GEOSGetGeometryN_r"
        sr
        (\hCtx -> c_GEOSGetGeometryN_r hCtx h (fromIntegral n))

-- |Gets the number of geometries in a 'Geometry' instance
getNumGeometries :: Geometry -> IO Int
getNumGeometries (Geometry sr _ h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSGetNumGeometries_r hCtx h
    if value == -1
       then throwGEOSError "GEOSGetNumGeometries_r"
       else return $ fromIntegral value

-- |Gets an ordinate value from a coordinate sequence
getOrdinate :: CoordinateSequence -> Word -> Word -> IO Double
getOrdinate coords idx dim =
    getOrdinateHelper
        "GEOSCoordSeq_getOrdinate_r"
        (\hCtx h idx' -> c_GEOSCoordSeq_getOrdinate_r hCtx h idx' (fromIntegral dim))
        coords
        idx

getOrdinateHelper :: String -> (GEOSContextHandle -> GEOSCoordSequencePtr -> CUInt -> Ptr CDouble -> IO CInt) ->
    CoordinateSequence ->
    Word ->
    IO Double
getOrdinateHelper context f (CoordinateSequence sr _ h) idx = do
    ContextState{..} <- readIORef sr
    alloca $ \valuePtr -> do
        status <- f hCtx h (fromIntegral idx) valuePtr
        case status of
             0 -> throwGEOSError context
             _ -> do
                value <- peek valuePtr
                return $ realToFrac value

-- |Gets the size from a coordinate sequence
getSize :: CoordinateSequence -> IO Word
getSize (CoordinateSequence sr _ h) = do
    ContextState{..} <- readIORef sr
    alloca $ \sizePtr -> do
        status <- c_GEOSCoordSeq_getSize_r hCtx h sizePtr
        case status of
             0 -> throwGEOSError "GEOSCoordSeq_getSize_r"
             _ -> do
                 size <- peek sizePtr
                 return $ fromIntegral size

-- |Gets an x-ordinate value from a coordinate sequence
getX :: CoordinateSequence -> Word -> IO Double
getX = getOrdinateHelper "GEOSCoordSeq_getX_r" c_GEOSCoordSeq_getX_r

-- |Gets a y-ordinate value from a coordinate sequence
getY :: CoordinateSequence -> Word -> IO Double
getY = getOrdinateHelper "GEOSCoordSeq_getY_r" c_GEOSCoordSeq_getY_r

-- |Gets a z-ordinate value from a coordinate sequence
getZ :: CoordinateSequence -> Word -> IO Double
getZ = getOrdinateHelper "GEOSCoordSeq_getZ_r" c_GEOSCoordSeq_getZ_r

-- |Returns a 'Geometry' instance representing the intersection of the two
-- supplied 'Geometry' instances:
intersection :: Geometry -> Geometry -> IO Geometry
intersection (Geometry sr0 _ h0) (Geometry sr1 _ h1) =
    checkAndTrackGeometry
        "GEOSIntersection_r"
        sr0
        (\hCtx -> c_GEOSIntersection_r hCtx h0 h1)

-- |Returns value indicating if specified 'Geometry' instance is empty
isEmpty :: Geometry -> IO Bool
isEmpty (Geometry sr _ h) = do
    ContextState{..} <- readIORef sr
    value <- c_GEOSisEmpty_r hCtx h
    case value of
         0 -> return False
         1 -> return True
         _ -> throwGEOSError "GEOSisEmpty_r"

mkContext :: IO Context
mkContext = do
    hCtx <- c_initializeGEOSWithHandlers
    sr <- newIORef $ ContextState hCtx []
    return $ Context sr

-- |Creates a reader used to deserialize 'Geometry' instances from
-- <https://en.wikipedia.org/wiki/Well-known_text WKT>-format text:
mkReader :: Context -> IO Reader
mkReader (Context sr) =
    checkAndTrack
        "GEOSWKTReader_create_r"
        sr
        c_GEOSWKTReader_create_r
        c_GEOSWKTReader_destroy_r
        Reader

-- |Creates a writer used to serialize 'Geometry' instances to
-- <https://en.wikipedia.org/wiki/Well-known_text WKT>-format text:
mkWriter :: Context -> IO Writer
mkWriter (Context sr) =
    checkAndTrack
        "GEOSWKTWriter_destroy_r"
        sr
        c_GEOSWKTWriter_create_r
        c_GEOSWKTWriter_destroy_r
        Writer

-- |Deserializes a 'Geometry' instance from the given 'String' using the
-- supplied 'Reader':
readGeometry :: Reader -> String -> IO Geometry
readGeometry (Reader sr _ h) str = withCString str $ \cs -> do
    checkAndTrackGeometry
        "GEOSWKTReader_read_r"
        sr
        (\hCtx -> c_GEOSWKTReader_read_r hCtx h cs)

releaseContext :: Context -> IO ()
releaseContext (Context sr) = do
    ContextState{..} <- readIORef sr
    mapM_ (\(DeleteAction _ f) -> f) deleteActions
    c_finishGEOS_r hCtx

-- |Sets an x-ordinate value within a coordinate sequence
setOrdinate :: CoordinateSequence -> Word -> Word -> Double -> IO ()
setOrdinate coords idx dim =
    setOrdinateHelper
        "GEOSCoordSeq_setOrdinate_r"
        (\hCtx h idx' -> c_GEOSCoordSeq_setOrdinate_r hCtx h idx' (fromIntegral dim))
        coords
        idx

setOrdinateHelper :: String -> (GEOSContextHandle -> GEOSCoordSequencePtr -> CUInt -> CDouble -> IO CInt ) -> CoordinateSequence -> Word -> Double -> IO ()
setOrdinateHelper context f (CoordinateSequence sr _ h) idx val = do
    ContextState{..} <- readIORef sr
    status <- f hCtx h (fromIntegral idx) (realToFrac val)
    if status == 0
       then throwGEOSError context
       else return ()

-- |Sets an x-ordinate value within a coordinate sequence
setX :: CoordinateSequence -> Word -> Double -> IO ()
setX = setOrdinateHelper "GEOSCoordSeq_setX_r" c_GEOSCoordSeq_setX_r

-- |Sets a y-ordinate value within a coordinate sequence
setY :: CoordinateSequence -> Word -> Double -> IO ()
setY = setOrdinateHelper "GEOSCoordSeq_setY_r" c_GEOSCoordSeq_setY_r

-- |Sets a z-ordinate value within a coordinate sequence
setZ :: CoordinateSequence -> Word -> Double -> IO ()
setZ = setOrdinateHelper "GEOSCoordSeq_setZ_r" c_GEOSCoordSeq_setZ_r

untrack :: ContextStateRef -> [DeleteAction] -> IO ()
untrack sr deleteActions = do
    let rawPtrs = map (\(DeleteAction rawPtr _) -> rawPtr) deleteActions
    modifyIORef' sr $ \p@ContextState{..} -> p { deleteActions = filter (\(DeleteAction rawPtr _) -> not (rawPtr `elem` rawPtrs)) deleteActions }

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
writeGeometry (Writer sr _ hWriter) (Geometry _ _ hGeometry) = do
    ContextState{..} <- readIORef sr
    bracket
        (c_GEOSWKTWriter_write_r hCtx hWriter hGeometry)
        (c_GEOSFree_r_CString hCtx)
        (\cs -> if cs == nullPtr then throwGEOSError "GEOSWKTWriter_write_r" else peekCString cs)
