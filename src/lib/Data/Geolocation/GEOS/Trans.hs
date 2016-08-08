{-|
Module      : Data.Geolocation.GEOS.Trans
Description : Monad transformer wrappers for high-level GEOS API
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

These are monad transformer wrapper functions for the high-level API to
simplify error handling in client code.

For the low-level FFI bindings, see "Data.Geolocation.GEOS.Imports".

For the high-level API, see "Data.Geolocation.GEOS".

<https://github.com/rcook/hgeos/blob/master/src/test/GEOSTest/TransAPI.hs View sample>
-}

module Data.Geolocation.GEOS.Trans
    ( GEOSError (..)
    , MonadGEOS
    , areaM
    , createCollectionM
    , createCoordSeqM
    , createEmptyPolygonM
    , createLinearRingM
    , createPolygonM
    , envelopeM
    , geomTypeIdM
    , getCoordSeqM
    , getExteriorRingM
    , getGeometryM
    , getNumGeometriesM
    , getOrdinateM
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
    , setOrdinateM
    , setXM
    , setYM
    , setZM
    , writeGeometryM
    ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Geolocation.GEOS
import Data.Word

data GEOSError = GEOSError String deriving Show

note :: IO (Maybe a) -> IO (Either GEOSError a)
note action = do
    result <- action
    case result of
         Nothing -> getErrorMessage >>= return . Left . GEOSError
         Just value -> return $ Right value

type MonadGEOS a = ExceptT GEOSError IO a

unaryGEOSFunc :: (a -> IO (Maybe b)) -> a -> MonadGEOS b
unaryGEOSFunc f a = ExceptT $ note (f a)

binaryGEOSFunc :: (a -> b -> IO (Maybe c)) -> a -> b -> MonadGEOS c
binaryGEOSFunc f a b = ExceptT $ note (f a b)

ternaryGEOSFunc :: (a -> b -> c -> IO (Maybe d)) -> a -> b -> c -> MonadGEOS d
ternaryGEOSFunc f a b c = ExceptT $ note (f a b c)

quaternaryGEOSFunc :: (a -> b -> c -> d -> IO (Maybe e)) -> a -> b -> c -> d -> MonadGEOS e
quaternaryGEOSFunc f a b c d = ExceptT $ note (f a b c d)

-- |@MonadGEOS@-wrapped version of 'area'
areaM :: Geometry -> MonadGEOS Double
areaM = unaryGEOSFunc area

-- |@MonadGEOS@-wrapped version of 'createCollection'
createCollectionM :: GeometryType -> [Geometry] -> MonadGEOS Geometry
createCollectionM = binaryGEOSFunc createCollection

-- |@MonadGEOS@-wrapped version of 'createCoordSeq'
createCoordSeqM :: Context -> Word -> Word -> MonadGEOS CoordinateSequence
createCoordSeqM = ternaryGEOSFunc createCoordSeq

-- |@MonadGEOS@-wrapped version of 'createEmptyPolygon'
createEmptyPolygonM :: Context -> MonadGEOS Geometry
createEmptyPolygonM = unaryGEOSFunc createEmptyPolygon

-- |@MonadGEOS@-wrapped version of 'createLinearRing'
createLinearRingM :: CoordinateSequence -> MonadGEOS Geometry
createLinearRingM = unaryGEOSFunc createLinearRing

-- |@MonadGEOS@-wrapped version of 'createPolygon'
createPolygonM :: Geometry -> [Geometry] -> MonadGEOS Geometry
createPolygonM = binaryGEOSFunc createPolygon

-- |@MonadGEOS@-wrapped version of 'envelope'
envelopeM :: Geometry -> MonadGEOS Geometry
envelopeM = unaryGEOSFunc envelope

-- |@MonadGEOS@-wrapped version of 'geomTypeId'
geomTypeIdM :: Geometry -> MonadGEOS GeometryType
geomTypeIdM = unaryGEOSFunc geomTypeId

-- |@MonadGEOS@-wrapped version of 'getCoordSeq'
getCoordSeqM :: Geometry -> MonadGEOS CoordinateSequence
getCoordSeqM = unaryGEOSFunc getCoordSeq

-- |@MonadGEOS@-wrapped version of 'getExteriorRing'
getExteriorRingM :: Geometry -> MonadGEOS Geometry
getExteriorRingM = unaryGEOSFunc getExteriorRing

-- |@MonadGEOS@-wrapped version of 'getGeometry'
getGeometryM :: Geometry -> Int -> MonadGEOS Geometry
getGeometryM = binaryGEOSFunc getGeometry

-- |@MonadGEOS@-wrapped version of 'getNumGeometries'
getNumGeometriesM :: Geometry -> MonadGEOS Int
getNumGeometriesM = unaryGEOSFunc getNumGeometries

-- |@MonadGEOS@-wrapped version of 'getOrdinate'
getOrdinateM :: CoordinateSequence -> Word -> Word -> MonadGEOS Double
getOrdinateM = ternaryGEOSFunc getOrdinate

-- |@MonadGEOS@-wrapped version of 'getSize'
getSizeM :: CoordinateSequence -> MonadGEOS Word
getSizeM = unaryGEOSFunc getSize

-- |@MonadGEOS@-wrapped version of 'getX'
getXM :: CoordinateSequence -> Word -> MonadGEOS Double
getXM = binaryGEOSFunc getX

-- |@MonadGEOS@-wrapped version of 'getY'
getYM :: CoordinateSequence -> Word -> MonadGEOS Double
getYM = binaryGEOSFunc getY

-- |@MonadGEOS@-wrapped version of 'getZ'
getZM :: CoordinateSequence -> Word -> MonadGEOS Double
getZM = binaryGEOSFunc getZ

-- |@MonadGEOS@-wrapped version of 'intersection'
intersectionM :: Geometry -> Geometry -> MonadGEOS Geometry
intersectionM = binaryGEOSFunc intersection

-- |@MonadGEOS@-wrapped version of 'isEmpty'
isEmptyM :: Geometry -> MonadGEOS Bool
isEmptyM = unaryGEOSFunc isEmpty

-- |@MonadGEOS@-wrapped version of 'mkReader'
mkReaderM :: Context -> MonadGEOS Reader
mkReaderM = unaryGEOSFunc mkReader

-- |@MonadGEOS@-wrapped version of 'mkWriter'
mkWriterM :: Context -> MonadGEOS Writer
mkWriterM = unaryGEOSFunc mkWriter

-- |@MonadGEOS@-wrapped version of 'readGeometry'
readGeometryM :: Reader -> String -> MonadGEOS Geometry
readGeometryM = binaryGEOSFunc readGeometry

-- |Creates a <https://trac.osgeo.org/geos/ GEOS> context, passes it to a block
-- and releases the context and all associated objects such as readers, writers
-- and geometries at the end inside a @MonadGEOS@ monad returning a message in
-- case of error:
--
-- @
--    result <- runGEOS $ \ctx -> do
--
--        -- Use context
--
--        return ()
--    case result of
--          Left (GEOSError m) -> putStrLn $ "Failed: " ++ m
--          Right r -> putStrLn $ "Succeeded: " ++ show r
-- @
runGEOS :: (Context -> MonadGEOS a) -> IO (Either GEOSError a)
runGEOS = withGEOS . (runExceptT .)

-- |@MonadGEOS@-wrapped version of 'setOrdinate'
setOrdinateM = quaternaryGEOSFunc setOrdinate

-- |@MonadGEOS@-wrapped version of 'setX'
setXM = ternaryGEOSFunc setX

-- |@MonadGEOS@-wrapped version of 'setY'
setYM = ternaryGEOSFunc setY

-- |@MonadGEOS@-wrapped version of 'setZ'
setZM = ternaryGEOSFunc setZ

-- |@MonadGEOS@-wrapped version of 'area'
writeGeometryM :: Writer -> Geometry -> MonadGEOS String
writeGeometryM = binaryGEOSFunc writeGeometry
