module Data.Geocoding.GEOS.LowLevelAPI
    ( module Data.Geocoding.GEOS.LowLevelImports
    , withGEOS
    , withWKTReader
    , withWKTWriter
    , wrap
    ) where

import Control.Exception
import Data.Geocoding.GEOS.LowLevelImports
import Foreign.Ptr

withGEOS :: (GEOSContextHandle_t -> IO a) -> IO a
withGEOS = bracket c_initializeGEOSWithHandlers c_uninitializeGEOS

withWKTReader :: GEOSContextHandle_t -> (GEOSWKTReaderPtr -> IO a) -> IO a
withWKTReader h = bracket (c_GEOSWKTReader_create_r h) (c_GEOSWKTReader_destroy_r h)

withWKTWriter :: GEOSContextHandle_t -> (GEOSWKTWriterPtr -> IO a) -> IO a
withWKTWriter h = bracket (c_GEOSWKTWriter_create_r h) (c_GEOSWKTWriter_destroy_r h)

wrap :: GEOSGeometryPtr -> Maybe GEOSGeometryPtr
wrap g@(GEOSGeometryPtr p) = if p == nullPtr then Nothing else Just g
