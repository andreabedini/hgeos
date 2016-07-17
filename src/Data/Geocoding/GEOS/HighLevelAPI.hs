module Data.Geocoding.GEOS.HighLevelAPI
    ( module Data.Geocoding.GEOS.HighLevelImports
    , withContext
    ) where

import Control.Exception
import Data.Geocoding.GEOS.HighLevelImports

withContext :: (ContextPtr -> IO a) -> IO a
withContext = bracket c_createContext c_contextDestroy
