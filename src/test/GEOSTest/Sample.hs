{-# LANGUAGE RecordWildCards #-}

module GEOSTest.Sample (demo) where

import Control.Monad
import Data.Geolocation.GEOS
import Data.List
import Data.Maybe
import Data.String.Utils
import GEOSTest.Arith
import Paths_hgeos
import Text.Printf

type Coordinate = (Double, Double, Double)
type Latitude = Double
type Longitude = Double
type Resolution = Double

data Extent = Extent
    { minX :: Double
    , maxX :: Double
    , minY :: Double
    , maxY :: Double
    , minZ :: Double
    , maxZ :: Double
    } deriving Show

getXYZs :: CoordinateSequence -> IO [Coordinate]
getXYZs coordSeq = do
    size <- getSize coordSeq
    forM [0..(size - 1)] $ \i -> do
        x <- getX coordSeq i
        y <- getY coordSeq i
        z <- getZ coordSeq i
        return (x, y, z)

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

-- TODO: Build polygon using API instead of string parsing
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
    mapM (getGeometry geometry) [0..(count - 1)]

findBiggestPolygon :: Geometry -> IO Geometry
findBiggestPolygon geometry = do
    gs@(gHead : gTail) <- getGeometries geometry
    as@(aHead : aTail) <- mapM area gs
    return $ fst $ foldr
        (\p'@(_, a') p@(_, a) -> if a' > a then p' else p)
        (gHead, aHead)
        (zip gTail aTail)

processPolygon :: String -> Resolution -> Longitude -> Latitude -> Geometry -> IO ()
processPolygon tableName resolution longitude latitude p = do
    shell <- getExteriorRing p
    coordSeq <- getCoordSeq shell
    xyzs <- getXYZs coordSeq
    let pId = polygonId resolution longitude latitude
        format :: Int -> Double -> Double -> String
        format = printf "INSERT INTO %s (polygon_id, point_id, longitude, latitude) VALUES ('%s', %d, %f, %f);\n" tableName pId
        strs = map (\(pointId, (x, y, _)) -> format pointId x y) (zip [0..] xyzs)
    forM_ strs putStr

polygonId :: Resolution -> Longitude -> Latitude -> String
polygonId resolution longitude latitude = "id_" ++ formatValue longitude ++ "_" ++ formatValue latitude
    where
        formatValue :: Double -> String
        formatValue = replace "-" "m" . replace "." "_" . (printf "%.2f") . mfloor resolution

generatePolygonMeshSQL :: Context -> String -> Resolution -> IO ()
generatePolygonMeshSQL ctx wkt resolution = do
    reader <- mkReader ctx
    writer <- mkWriter ctx
    country <- readGeometry reader wkt
    env <- envelope country
    shell <- getExteriorRing env
    coordSeq <- getCoordSeq shell
    xyzs <- getXYZs coordSeq

    putStrLn "Shell:"
    mapM_ print xyzs

    let Extent{..} = extent xyzs
        mfloorRes = mfloor resolution
        longitudeBegin = mfloorRes minX
        longitudeEnd = mfloorRes maxX + resolution
        latitudeBegin = mfloorRes minY
        latitudeEnd = mfloorRes maxY + resolution

    putStrLn "Longitude and latitude ranges:"
    putStrLn $ printf "  longitude %f to %f" longitudeBegin longitudeEnd
    putStrLn $ printf "  latitude %f to %f" latitudeBegin latitudeEnd

    let longitudes = frange longitudeBegin longitudeEnd 1.0
        latitudes = frange latitudeBegin latitudeEnd 1.0
    forM_ [(i, j) | i <- longitudes, j <- latitudes] $ \(longitude, latitude) -> do
        square <- mkSquare reader longitude latitude resolution
        overlap <- intersection square country
        isOverlapEmpty <- isEmpty overlap
        unless isOverlapEmpty $ do
            typeId <- geomTypeId overlap
            polygon <- case typeId of
                            MultiPolygon -> findBiggestPolygon overlap
                            Polygon -> return overlap
            processPolygon "TABLE_NAME" resolution longitude latitude polygon
    return ()

demo :: IO ()
demo = do
    fileName <- getDataFileName "data/namibia.wkt"
    wkt <- readFile fileName
    withGEOS $ \ctx -> generatePolygonMeshSQL ctx wkt 1.0
    putStrLn "Sample.demo: succeeded"
