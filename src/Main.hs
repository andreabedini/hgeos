module Main (main) where

import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.Ptr

newtype GEOSContextHandle_t = GEOSContextHandle_t (Ptr GEOSContextHandle_t)
newtype GEOSWKTReaderPtr = GEOSWKTReaderPtr (Ptr GEOSWKTReaderPtr)
newtype GEOSGeometryPtr = GEOSGeometryPtr (Ptr GEOSGeometryPtr) deriving Show

foreign import ccall "GEOSversion" c_GEOSversion :: CString

foreign import ccall "helpers.h initializeGEOSWithHandlers"
    c_initializeGEOSWithHandlers :: IO GEOSContextHandle_t
foreign import ccall "helpers.h uninitializeGEOS"
    c_uninitializeGEOS :: GEOSContextHandle_t -> IO ()
foreign import ccall "helpers.h getNoticeMessage"
    c_getNoticeMessage :: IO CString
foreign import ccall "helpers.h getErrorMessage"
    c_getErrorMessage :: IO CString

foreign import ccall "GEOSWKTReader_create_r" c_GEOSWKTReader_create_r :: GEOSContextHandle_t -> GEOSWKTReaderPtr
foreign import ccall "GEOSWKTReader_destroy_r" c_GEOSWKTReader_destroy_r :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> ()
foreign import ccall "GEOSWKTReader_read_r" c_GEOSWKTReader_read_r ::
    GEOSContextHandle_t ->
    GEOSWKTReaderPtr ->
    CString ->
    GEOSGeometryPtr
readGeometry :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> CString -> Maybe GEOSGeometryPtr
readGeometry h r wkt =
    let g = c_GEOSWKTReader_read_r h r wkt
    in if isNull g then Nothing else Just g
    where
        isNull :: GEOSGeometryPtr -> Bool
        isNull (GEOSGeometryPtr p) = p == nullPtr

foreign import ccall
    "GEOSIntersection_r" c_GEOSIntersection_r ::
    GEOSContextHandle_t ->
    GEOSGeometryPtr ->
    GEOSGeometryPtr ->
    GEOSGeometryPtr

foreign import ccall "helpers.h multiply" c_multiply :: CInt -> CInt -> CInt

withGEOS :: (GEOSContextHandle_t -> IO a) -> IO a
withGEOS = bracket c_initializeGEOSWithHandlers c_uninitializeGEOS

main :: IO ()
main = do
    let cs = c_GEOSversion
    s <- peekCString cs
    putStrLn s

    wkt0 <- newCString "POLYGON (( 10.1 10, 10 20, 20 20, 20 10, 10 10 ))"
    wkt1 <- newCString "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"

    void $ withGEOS $ \h -> do
        let r = c_GEOSWKTReader_create_r h
            g0 = readGeometry h r wkt0
            g1 = readGeometry h r wkt1
            --_ = c_GEOSWKTReader_destroy_r handle r
            --g2 = c_GEOSIntersection_r handle g0 g1
        print g0
        print g1
        --print g2
    putStrLn "Done"
