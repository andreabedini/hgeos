{-# LANGUAGE RecordWildCards #-}

module GEOSTest.Sample (demo) where

import Control.Monad
import Data.Geolocation.GEOS
import Data.List
import Data.String.Utils
import GEOSTest.Arith
import Paths_hgeos
import Text.Printf

type Coordinate = (Double, Double, Double)
type Latitude = Double
type Longitude = Double
type Resolution = Double

printGeometry :: Writer -> Geometry -> IO ()
printGeometry r g = writeGeometry r g >>= putStrLn

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

mkSquare :: Reader -> Longitude -> Latitude -> Resolution -> IO Geometry
mkSquare reader longitude latitude resolution = do
    let points = [
            (longitude, latitude),
            (longitude + resolution, latitude),
            (longitude + resolution, latitude + resolution),
            (longitude, latitude + resolution),
            (longitude, latitude)
            ]
        wkt = printf "POLYGON ((%s))" (intercalate "," (map (\(a, b) -> printf "%f %f" a b) points))
    (Just g) <- readGeometry reader wkt
    return g

getGeometries :: Geometry -> IO [Geometry]
getGeometries geometry = do
    count <- getNumGeometries geometry
    forM [0..(count - 1)] $ \i -> do
        (Just g) <- getGeometry geometry i
        return g

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
    (Just shell) <- exteriorRing p
    (Just coordSeq) <- coordinateSequence shell
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

-- A more involved example of use of API
demo :: IO ()
demo = do
    fileName <- getDataFileName "data/namibia.wkt"
    wkt <- readFile fileName
    withGEOS $ \ctx -> do
        reader <- mkReader ctx
        writer <- mkWriter ctx
        let p = printGeometry writer

        (Just country) <- readGeometry reader wkt
        (Just env) <- envelope country
        (Just shell) <- exteriorRing env
        (Just coordSeq) <- coordinateSequence shell
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
            (Just overlap) <- intersection square country
            x <- isEmpty overlap
            unless x $ do
                t <- geometryType overlap
                polygon <- case t of
                                MultiPolygon -> findBiggestPolygon overlap
                                Polygon -> return overlap
                processPolygon "foo" resolution longitude latitude polygon
    putStrLn "Sample.demo done"
