module Data.Geocoding.GEOS.HighLevelAPI
    ( withContext
    , c_contextCreateReader
    , c_contextCreateWriter
    , c_contextIntersection
    , c_readerRead
    , c_writerWrite
    ) where

import Control.Exception
import Data.Geocoding.GEOS.HighLevelImports

withContext :: (ContextPtr -> IO a) -> IO a
withContext = bracket c_createContext c_contextDestroy
