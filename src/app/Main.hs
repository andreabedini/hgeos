{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Geolocation.GEOS
import Data.Geolocation.GEOS.Imports
import Data.List
import Data.String.Utils
import Foreign.C
import Foreign.Ptr
import Paths_hgeos
import Text.Printf

-- Demonstrates direct use of imports
-- Lifetimes of various GEOS objects, including readers, writers and
-- geometries, must be managed explicitly by clients using explicit calls to
-- various destroy/free functions
lowLevelAPIDemo :: IO ()
lowLevelAPIDemo = do
    wkt0 <- newCString "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
    wkt1 <- newCString "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"

    withGEOS $ \ctx -> do
        withWKTReader ctx $ \reader -> do
            withGeometry ctx reader wkt0 $ \g0 -> do
                withGeometry ctx reader wkt1 $ \g1 -> do
                    bracket (c_GEOSIntersection_r ctx g0 g1) (c_GEOSGeom_destroy_r ctx) $ \g2 -> do
                        withWKTWriter ctx $ \writer -> do
                            str <- bracket
                                (c_GEOSWKTWriter_write_r ctx writer g2)
                                (c_GEOSFree_r_CString ctx)
                                peekCString
                            putStrLn str
                            putStrLn "lowLevelAPIDemo done"
    where
        withGEOS :: (GEOSContextHandle_t -> IO a) -> IO a
        withGEOS = bracket c_initializeGEOSWithHandlers c_finishGEOS_r
        withWKTReader :: GEOSContextHandle_t -> (GEOSWKTReaderPtr -> IO a) -> IO a
        withWKTReader ctx = bracket (c_GEOSWKTReader_create_r ctx) (c_GEOSWKTReader_destroy_r ctx)
        withWKTWriter :: GEOSContextHandle_t -> (GEOSWKTWriterPtr -> IO a) -> IO a
        withWKTWriter ctx = bracket (c_GEOSWKTWriter_create_r ctx) (c_GEOSWKTWriter_destroy_r ctx)
        withGeometry :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> CString -> (GEOSGeometryPtr -> IO a) -> IO a
        withGeometry ctx reader wkt =
            bracket
                (c_GEOSWKTReader_read_r ctx reader wkt)
                (c_GEOSGeom_destroy_r ctx)

-- Demonstrates use of high-level API
-- Lifetimes of GEOS objects are automatically managed by the context objects
-- which guarantees that they are released when the context goes out of scope
highLevelAPIDemo :: IO ()
highLevelAPIDemo = do
    withContext $ \ctx -> do
        reader <- mkReader ctx
        g0 <- readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- readGeometry reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- intersection g0 g1
        writer <- mkWriter ctx
        str <- writeGeometry writer g2
        putStrLn str
        putStrLn "highLevelAPIDemo done"

main :: IO ()
main = do
    s <- peekCString c_GEOSversion
    putStrLn s
    lowLevelAPIDemo
    highLevelAPIDemo
    namibiaDemo

printGeometry :: Writer -> Geometry -> IO ()
printGeometry r g = writeGeometry r g >>= putStrLn

type Coordinate = (Double, Double, Double)

getXYZs :: CoordinateSequence -> IO (Maybe [Coordinate])
getXYZs coordSeq = do
    maybeSize <- getSize coordSeq
    case maybeSize of
         Nothing -> return Nothing
         Just size -> do
             xyzs <- forM [0..(size - 1)] $ \i -> do
                 (Just x) <- getX coordSeq i
                 (Just y) <- getY coordSeq i
                 (Just z) <- getZ coordSeq i
                 return (x, y, z)
             return $ Just xyzs

data Extent = Extent
    { minX :: Double
    , maxX :: Double
    , minY :: Double
    , maxY :: Double
    , minZ :: Double
    , maxZ :: Double
    } deriving Show

extent :: [Coordinate] -> Extent
extent ((x, y, z) : cs) =
    let (minX, maxX, minY, maxY, minZ, maxZ) =
            foldr (\(x', y', z') (minX, maxX, minY, maxY, minZ, maxZ) ->
                   (if x' < minX then x' else minX,
                    if x' > maxX then x' else maxX,
                    if y' < minY then y' else minY,
                    if y' > maxY then y' else maxY,
                    if z' < minZ then z' else minZ,
                    if z' > maxX then z' else maxZ)) (x, x, y, y, z, z) cs
    in Extent minX maxX minY maxY minZ maxZ

mfloor :: Double -> Double -> Double
mfloor m x = (fromInteger $ floor (x / m)) * m

frange :: Double -> Double -> Double -> [Double]
frange lower upper step =
    let count = (upper - lower) / step
    in [lower + (fromIntegral i) * step | i <- [0..(round count - 1)]]

type Longitude = Double
type Latitude = Double
type Resolution = Double

mkSquare :: Reader -> Longitude -> Latitude -> Resolution -> IO Geometry
mkSquare reader longitude latitude resolution =
    let points = [
            (longitude, latitude),
            (longitude + resolution, latitude),
            (longitude + resolution, latitude + resolution),
            (longitude, latitude + resolution),
            (longitude, latitude)
            ]
        wkt = printf "POLYGON ((%s))" (intercalate "," (map (\(a, b) -> printf "%f %f" a b) points))
    in readGeometry reader wkt

getGeometries :: Geometry -> IO [Geometry]
getGeometries geometry = do
    count <- getNumGeometries geometry
    forM [0..(count - 1)] (getGeometry geometry)

findBiggestPolygon :: Geometry -> IO Geometry
findBiggestPolygon geometry = do
    geometries@(g : gs) <- getGeometries geometry
    a : as <- sequence $ map area geometries
    let (g', _) = foldr (\p@(_, a') biggest@(_, aBiggest) -> if a' > aBiggest then p else biggest) (g, a) (zip gs as)
    return g'

resolution :: Double
resolution = 1.0

processPolygon :: String -> Resolution -> Longitude -> Latitude -> Geometry -> IO ()
processPolygon tableName resolution longitude latitude p = do
    let pId = polygonId resolution longitude latitude
    shell <- exteriorRing p
    coordSeq <- coordinateSequence shell
    (Just xyzs) <- getXYZs coordSeq
    forM_ (zip [0..] xyzs) $ \(pointId, (x, y, _)) -> do
        let s = printf
                    "INSERT INTO %s (polygon_id, point_id, longitude, latitude) VALUES ('%s', %d, %f, %f);\n"
                    tableName
                    pId
                    (pointId :: Int)
                    x
                    y
        putStr s

polygonId :: Resolution -> Longitude -> Latitude -> String
polygonId resolution longitude latitude = "id_" ++ formatValue longitude ++ "_" ++ formatValue latitude
    where
        formatValue :: Double -> String
        formatValue = replace "-" "m" . replace "." "_" . (printf "%.2f") . mfloor resolution

namibiaDemo :: IO ()
namibiaDemo = do
    fileName <- getDataFileName "data/namibia.wkt"
    wkt <- readFile fileName
    withContext $ \ctx -> do
        reader <- mkReader ctx
        writer <- mkWriter ctx
        let p = printGeometry writer

        country <- readGeometry reader wkt
        env <- envelope country
        shell <- exteriorRing env
        coordSeq <- coordinateSequence shell
        (Just xyzs) <- getXYZs coordSeq
        forM_ xyzs $ \(x, y, z) -> print (x, y, z)
        let Extent{..} = extent xyzs
            mfloorRes = mfloor resolution
            longitudeBegin = mfloorRes minX
            longitudeEnd = mfloorRes maxX + resolution
            latitudeBegin = mfloorRes minY
            latitudeEnd = mfloorRes maxY + resolution
        print longitudeBegin
        print longitudeEnd
        print latitudeBegin
        print latitudeEnd
        let longitudes = frange longitudeBegin longitudeEnd 1.0
            latitudes = frange latitudeBegin latitudeEnd 1.0
        forM_ [(i, j) | i <- longitudes, j <- latitudes] $ \(longitude, latitude) -> do
            square <- mkSquare reader longitude latitude resolution
            overlap <- intersection square country
            x <- isEmpty overlap
            unless x $ do
                id <- geometryTypeId overlap
                polygon <- case id of
                                MultiPolygon -> findBiggestPolygon overlap
                                Polygon -> return overlap
                processPolygon "foo" resolution longitude latitude polygon
    putStrLn "namibiaDemo done"
