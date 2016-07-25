{-|
Module      : Data.Geolocation.GEOS.Trans
Description : @MaybeT@ wrappers for high-level GEOS API
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : POSIX

These are @MaybeT@ monad transformer wrapper functions for the high-level API
to simplify error handling in client code.

For the low-level FFI bindings, see "Data.Geolocation.GEOS.Imports".

For the high-level API, see "Data.Geolocation.GEOS".

<https://github.com/rcook/hgeos/blob/master/src/test/GEOSTest/TransAPI.hs View sample>
-}

module Data.Geolocation.GEOS.Trans
    ( areaM
    , envelopeM
    , geomTypeIdM
    , getCoordSeqM
    , getExteriorRingM
    , getGeometryM
    , getNumGeometriesM
    , getSizeM
    , getXM
    , getYM
    , getZM
    , intersectionM
    , isEmptyM
    , mkReaderM
    , mkWriterM
    , readGeometryM
    , runGEOS
    , writeGeometryM
    ) where

import Control.Monad.Trans.Maybe
import Data.Geolocation.GEOS

unaryGEOSFunc :: (a -> IO (Maybe b)) -> a -> MaybeT IO b
unaryGEOSFunc = (MaybeT .)

binaryGEOSFunc :: (a -> b -> IO (Maybe c)) -> a -> b -> MaybeT IO c
binaryGEOSFunc f a b = MaybeT (f a b)

-- | @MaybeT@-wrapped version of 'area'
areaM :: Geometry -> MaybeT IO Double
areaM = unaryGEOSFunc area

-- | @MaybeT@-wrapped version of 'envelope'
envelopeM :: Geometry -> MaybeT IO Geometry
envelopeM = unaryGEOSFunc envelope

-- | @MaybeT@-wrapped version of 'geomTypeId'
geomTypeIdM :: Geometry -> MaybeT IO GeometryType
geomTypeIdM = unaryGEOSFunc geomTypeId

-- | @MaybeT@-wrapped version of 'getCoordSeq'
getCoordSeqM :: Geometry -> MaybeT IO CoordinateSequence
getCoordSeqM = unaryGEOSFunc getCoordSeq

-- | @MaybeT@-wrapped version of 'getExteriorRing'
getExteriorRingM :: Geometry -> MaybeT IO Geometry
getExteriorRingM = unaryGEOSFunc getExteriorRing

-- | @MaybeT@-wrapped version of 'getGeometry'
getGeometryM :: Geometry -> Int -> MaybeT IO Geometry
getGeometryM = binaryGEOSFunc getGeometry

-- | @MaybeT@-wrapped version of 'getNumGeometries'
getNumGeometriesM :: Geometry -> MaybeT IO Int
getNumGeometriesM = unaryGEOSFunc getNumGeometries

-- | @MaybeT@-wrapped version of 'getSize'
getSizeM :: CoordinateSequence -> MaybeT IO Word
getSizeM = unaryGEOSFunc getSize

-- | @MaybeT@-wrapped version of 'getX'
getXM :: CoordinateSequence -> Word -> MaybeT IO Double
getXM = binaryGEOSFunc getX

-- | @MaybeT@-wrapped version of 'getY'
getYM :: CoordinateSequence -> Word -> MaybeT IO Double
getYM = binaryGEOSFunc getY

-- | @MaybeT@-wrapped version of 'getZ'
getZM :: CoordinateSequence -> Word -> MaybeT IO Double
getZM = binaryGEOSFunc getZ

-- | @MaybeT@-wrapped version of 'intersection'
intersectionM :: Geometry -> Geometry -> MaybeT IO Geometry
intersectionM = binaryGEOSFunc intersection

-- | @MaybeT@-wrapped version of 'isEmpty'
isEmptyM :: Geometry -> MaybeT IO Bool
isEmptyM = unaryGEOSFunc isEmpty

-- | @MaybeT@-wrapped version of 'mkReader'
mkReaderM :: Context -> MaybeT IO Reader
mkReaderM = unaryGEOSFunc mkReader

-- | @MaybeT@-wrapped version of 'mkWriter'
mkWriterM :: Context -> MaybeT IO Writer
mkWriterM = unaryGEOSFunc mkWriter

-- | @MaybeT@-wrapped version of 'readGeometry'
readGeometryM :: Reader -> String -> MaybeT IO Geometry
readGeometryM = binaryGEOSFunc readGeometry

-- |Creates a <https://trac.osgeo.org/geos/ GEOS> context, passes it to a block
-- and releases the context and all associated objects such as readers, writers
-- and geometries at the end inside a @MaybeT IO@ monad:
--
-- @
--    runGEOS $ \ctx -> do
--
--        -- Use context
--
--        return ()
-- @
runGEOS :: (Context -> MaybeT IO a) -> IO (Maybe a)
runGEOS = withGEOS . (runMaybeT .)

-- | @MaybeT@-wrapped version of 'area'
writeGeometryM :: Writer -> Geometry -> MaybeT IO String
writeGeometryM = binaryGEOSFunc writeGeometry
