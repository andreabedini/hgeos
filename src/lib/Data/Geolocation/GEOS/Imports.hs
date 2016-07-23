{-|
Module      : Data.Geolocation.GEOS.Imports
Description : FFI bindings for GEOS C API
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : POSIX

These are low-level FFI bindings for the GEOS C API derived from
<http://geos.osgeo.org/doxygen/geos__c_8h_source.html geos_c.h>. These enable
low-level access to the native functions for parts of the C API for which
high-level wrappers do not yet exist.

For the high-level API, see "Data.Geolocation.GEOS".

<https://github.com/rcook/hgeos/blob/master/src/test/GEOSTest/LowLevelAPI.hs View sample>
-}

module Data.Geolocation.GEOS.Imports
    ( GEOSContextHandle_t ()
    , GEOSCoordSequencePtr ()
    , GEOSGeometryPtr ()
    , GEOSWKTReaderPtr ()
    , GEOSWKTWriterPtr ()
    , NullablePtr (isNullPtr)
    , c_GEOSArea_r
    , c_GEOSCoordSeq_destroy_r
    , c_GEOSCoordSeq_getSize_r
    , c_GEOSCoordSeq_getX_r
    , c_GEOSCoordSeq_getY_r
    , c_GEOSCoordSeq_getZ_r
    , c_GEOSEnvelope_r
    , c_GEOSFree_r_CString
    , c_GEOSGeomTypeId_r
    , c_GEOSGeom_destroy_r
    , c_GEOSGeom_getCoordSeq_r
    , c_GEOSGetExteriorRing_r
    , c_GEOSGetGeometryN_r
    , c_GEOSGetNumGeometries_r
    , c_GEOSIntersection_r
    , c_GEOSWKTReader_create_r
    , c_GEOSWKTReader_destroy_r
    , c_GEOSWKTReader_read_r
    , c_GEOSWKTWriter_create_r
    , c_GEOSWKTWriter_destroy_r
    , c_GEOSWKTWriter_write_r
    , c_GEOSisEmpty_r
    , c_GEOSversion
    , c_finishGEOS_r
    , c_initializeGEOSWithHandlers
    ) where

import Foreign.C
import Foreign.Ptr

-- |Determines if given pointer is null
class NullablePtr a where
    -- |Returns @True@ if pointer is null, @False@ otherwise
    isNullPtr :: a -> Bool

-- |Wraps @GEOSContextHandle_t@
newtype GEOSContextHandle_t = GEOSContextHandle_t (Ptr GEOSContextHandle_t)

-- |Wraps @GEOSCoordSequence*@
newtype GEOSCoordSequencePtr = GEOSCoordSequencePtr (Ptr GEOSCoordSequencePtr)
instance NullablePtr GEOSCoordSequencePtr where
    isNullPtr (GEOSCoordSequencePtr p) = p == nullPtr

-- |Wraps @GEOSGeometry*@
newtype GEOSGeometryPtr = GEOSGeometryPtr (Ptr GEOSGeometryPtr)
instance NullablePtr GEOSGeometryPtr where
    isNullPtr (GEOSGeometryPtr p) = p == nullPtr

-- |Wraps @GEOSWKTReader*@
newtype GEOSWKTReaderPtr = GEOSWKTReaderPtr (Ptr GEOSWKTReaderPtr)
instance NullablePtr GEOSWKTReaderPtr where
    isNullPtr (GEOSWKTReaderPtr p) = p == nullPtr

-- |Wraps @GEOSWKTWriter*@
newtype GEOSWKTWriterPtr = GEOSWKTWriterPtr (Ptr GEOSWKTWriterPtr)
instance NullablePtr GEOSWKTWriterPtr where
    isNullPtr (GEOSWKTWriterPtr p) = p == nullPtr

-- |Wraps @GEOSArea_r@
foreign import ccall "GEOSArea_r"
    c_GEOSArea_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> Ptr CDouble -> IO CInt

-- |Wraps @GEOSCoordSeq_destroy_r@
foreign import ccall "GEOSCoordSeq_destroy_r"
    c_GEOSCoordSeq_destroy_r :: GEOSContextHandle_t -> GEOSCoordSequencePtr -> IO ()

-- |Wraps @GEOSCoordSeq_getSize_r@
foreign import ccall "GEOSCoordSeq_getSize_r"
    c_GEOSCoordSeq_getSize_r :: GEOSContextHandle_t -> GEOSCoordSequencePtr -> Ptr CUInt -> IO CInt

-- |Wraps @GEOSCoordSeq_getX_r@
foreign import ccall "GEOSCoordSeq_getX_r"
    c_GEOSCoordSeq_getX_r :: GEOSContextHandle_t -> GEOSCoordSequencePtr -> CUInt -> Ptr CDouble -> IO CInt

-- |Wraps @GEOSCoordSeq_getY_r@
foreign import ccall "GEOSCoordSeq_getY_r"
    c_GEOSCoordSeq_getY_r :: GEOSContextHandle_t -> GEOSCoordSequencePtr -> CUInt -> Ptr CDouble -> IO CInt

-- |Wraps @GEOSCoordSeq_getZ_r@
foreign import ccall "GEOSCoordSeq_getZ_r"
    c_GEOSCoordSeq_getZ_r :: GEOSContextHandle_t -> GEOSCoordSequencePtr -> CUInt -> Ptr CDouble -> IO CInt

-- |Wraps @GEOSEnvelope_r@
foreign import ccall "GEOSEnvelope_r"
    c_GEOSEnvelope_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO GEOSGeometryPtr

-- |Wraps @GEOSFree_r@ specialized to @const char*@
foreign import ccall "GEOSFree_r"
    c_GEOSFree_r_CString :: GEOSContextHandle_t -> CString -> IO ()

-- |Wraps @GEOSGeomTypeId_r@
foreign import ccall "GEOSGeomTypeId_r"
    c_GEOSGeomTypeId_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO CInt

-- |Wraps @GEOSGeom_destroy_r@
foreign import ccall "GEOSGeom_destroy_r"
    c_GEOSGeom_destroy_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO ()

-- |Wraps @GEOSGeom_getCoordSeq_r@
foreign import ccall "GEOSGeom_getCoordSeq_r"
    c_GEOSGeom_getCoordSeq_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO GEOSCoordSequencePtr

-- |Wraps @GEOSGetExteriorRing_r@
foreign import ccall "GEOSGetExteriorRing_r"
    c_GEOSGetExteriorRing_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO GEOSGeometryPtr

-- |Wraps @GEOSGetGeometryN_r@
foreign import ccall "GEOSGetGeometryN_r"
    c_GEOSGetGeometryN_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> CInt -> IO GEOSGeometryPtr

-- |Wraps @GEOSGetNumGeometries_r@
foreign import ccall "GEOSGetNumGeometries_r"
    c_GEOSGetNumGeometries_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO CInt

-- |Wraps @GEOSIntersection_r@
foreign import ccall "GEOSIntersection_r"
    c_GEOSIntersection_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> GEOSGeometryPtr -> IO GEOSGeometryPtr

-- |Wraps @GEOSWKTReader_create_r@
foreign import ccall "GEOSWKTReader_create_r"
    c_GEOSWKTReader_create_r :: GEOSContextHandle_t -> IO GEOSWKTReaderPtr

-- |Wraps @GEOSWKTReader_destroy_r@
foreign import ccall "GEOSWKTReader_destroy_r"
    c_GEOSWKTReader_destroy_r :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> IO ()

-- |Wraps @GEOSWKTReader_read_r@
foreign import ccall "GEOSWKTReader_read_r"
    c_GEOSWKTReader_read_r :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> CString -> IO GEOSGeometryPtr

-- |Wraps @GEOSWKTWriter_create_r@
foreign import ccall "GEOSWKTWriter_create_r"
    c_GEOSWKTWriter_create_r :: GEOSContextHandle_t -> IO GEOSWKTWriterPtr

-- |Wraps @GEOSWKTWriter_destroy_r@
foreign import ccall "GEOSWKTWriter_destroy_r"
    c_GEOSWKTWriter_destroy_r :: GEOSContextHandle_t -> GEOSWKTWriterPtr -> IO ()

-- |Wraps @GEOSWKTWriter_write_r@
foreign import ccall "GEOSWKTWriter_write_r"
    c_GEOSWKTWriter_write_r :: GEOSContextHandle_t -> GEOSWKTWriterPtr -> GEOSGeometryPtr -> IO CString

-- |Wraps @GEOSisEmpty_r@
foreign import ccall "GEOSisEmpty_r"
    c_GEOSisEmpty_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO CChar

-- |Wraps @GEOSversion@
foreign import ccall "GEOSversion"
    c_GEOSversion :: IO CString

-- |Wraps @finishGEOS_r@ helper function
foreign import ccall "finishGEOS_r"
    c_finishGEOS_r :: GEOSContextHandle_t -> IO ()

-- |Wraps @getErrorMessage@ helper function
foreign import ccall "helpers.h getErrorMessage"
    c_getErrorMessage :: IO CString

-- |Wraps @getNoticeMessage@ helper function
foreign import ccall "helpers.h getNoticeMessage"
    c_getNoticeMessage :: IO CString

-- |Wraps @initializeGEOSWithHandlers@ helper function
foreign import ccall "helpers.h initializeGEOSWithHandlers"
    c_initializeGEOSWithHandlers :: IO GEOSContextHandle_t
