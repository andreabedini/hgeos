module Data.Geocoding.GEOS.Imports where

import Foreign.C
import Foreign.Ptr

newtype GEOSContextHandle_t = GEOSContextHandle_t (Ptr GEOSContextHandle_t)
newtype GEOSGeometryPtr = GEOSGeometryPtr (Ptr GEOSGeometryPtr)
newtype GEOSWKTReaderPtr = GEOSWKTReaderPtr (Ptr GEOSWKTReaderPtr)
newtype GEOSWKTWriterPtr = GEOSWKTWriterPtr (Ptr GEOSWKTWriterPtr)

foreign import ccall "GEOSversion"
    c_GEOSversion :: CString

foreign import ccall "GEOSFree_r"
    c_GEOSFree_r_CChar :: GEOSContextHandle_t -> Ptr CChar -> IO ()

foreign import ccall "helpers.h initializeGEOSWithHandlers"
    c_initializeGEOSWithHandlers :: IO GEOSContextHandle_t
foreign import ccall "helpers.h uninitializeGEOS"
    c_uninitializeGEOS :: GEOSContextHandle_t -> IO ()
foreign import ccall "helpers.h getNoticeMessage"
    c_getNoticeMessage :: IO CString
foreign import ccall "helpers.h getErrorMessage"
    c_getErrorMessage :: IO CString

foreign import ccall "GEOSGeom_destroy_r"
    c_GEOSGeom_destroy_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO ()

foreign import ccall "GEOSWKTReader_create_r"
    c_GEOSWKTReader_create_r :: GEOSContextHandle_t -> IO GEOSWKTReaderPtr
foreign import ccall "GEOSWKTReader_destroy_r"
    c_GEOSWKTReader_destroy_r :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> IO ()
foreign import ccall "GEOSWKTReader_read_r"
    c_GEOSWKTReader_read_r :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> CString -> IO GEOSGeometryPtr

foreign import ccall "GEOSWKTWriter_create_r"
    c_GEOSWKTWriter_create_r :: GEOSContextHandle_t -> IO GEOSWKTWriterPtr
foreign import ccall "GEOSWKTWriter_destroy_r"
    c_GEOSWKTWriter_destroy_r :: GEOSContextHandle_t -> GEOSWKTWriterPtr -> IO ()
foreign import ccall "GEOSWKTWriter_write_r"
    c_GEOSWKTWriter_write_r :: GEOSContextHandle_t -> GEOSWKTWriterPtr -> GEOSGeometryPtr -> IO CString

foreign import ccall "GEOSIntersection_r"
    c_GEOSIntersection_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> GEOSGeometryPtr -> IO GEOSGeometryPtr
