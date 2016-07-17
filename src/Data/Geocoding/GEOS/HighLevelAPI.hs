module Data.Geocoding.GEOS.HighLevelAPI
    ( envelope
    , intersection
    , mkReader
    , mkWriter
    , readGeometry
    , withContext
    , writeGeometry
    ) where

import Control.Exception
import Foreign.C
import Data.Geocoding.GEOS.HighLevelImports

withContext :: (ContextPtr -> IO a) -> IO a
withContext = bracket c_createContext c_contextDestroy

mkReader :: ContextPtr -> IO ReaderPtr
mkReader = c_contextCreateReader

readGeometry :: ReaderPtr -> String -> IO GeometryPtr
readGeometry r s = withCString s (c_readerRead r)

mkWriter :: ContextPtr -> IO WriterPtr
mkWriter = c_contextCreateWriter

writeGeometry :: WriterPtr -> GeometryPtr -> IO String
writeGeometry w g = c_writerWrite w g >>= peekCString

envelope :: GeometryPtr -> IO GeometryPtr
envelope = c_contextEnvelope

intersection :: GeometryPtr -> GeometryPtr -> IO GeometryPtr
intersection = c_contextIntersection
