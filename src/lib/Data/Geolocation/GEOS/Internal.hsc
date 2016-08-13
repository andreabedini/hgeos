{-|
Module      : Data.Geolocation.GEOS.Internal
Description : Imports from GEOS C API headers
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Low-level imports from <http://geos.osgeo.org/doxygen/geos__c_8h_source.html geos_c.h>
-}

module Data.Geolocation.GEOS.Internal
    ( GeometryType (GeometryType)
    , geometryCollection
    , lineString
    , linearRing
    , multiLineString
    , multiPoint
    , multiPolygon
    , point
    , polygon
    , unGeometryType
    ) where

import Foreign.C

#include <geos_c.h>

newtype GeometryType = GeometryType
    { unGeometryType :: CInt
    } deriving (Eq, Show)

#{enum GeometryType, GeometryType
    , point = GEOS_POINT
    , lineString = GEOS_LINESTRING
    , linearRing = GEOS_LINEARRING
    , polygon = GEOS_POLYGON
    , multiPoint = GEOS_MULTIPOINT
    , multiLineString = GEOS_MULTILINESTRING
    , multiPolygon = GEOS_MULTIPOLYGON
    , geometryCollection = GEOS_GEOMETRYCOLLECTION
}
