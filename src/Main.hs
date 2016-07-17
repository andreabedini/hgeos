{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.Ptr

newtype GEOSContextHandle_t = GEOSContextHandle_t (Ptr GEOSContextHandle_t)
newtype GEOSGeometryPtr = GEOSGeometryPtr (Ptr GEOSGeometryPtr) deriving Show
newtype GEOSWKTReaderPtr = GEOSWKTReaderPtr (Ptr GEOSWKTReaderPtr)
newtype GEOSWKTWriterPtr = GEOSWKTWriterPtr (Ptr GEOSWKTWriterPtr)

foreign import ccall "GEOSversion"
    c_GEOSversion :: CString

foreign import ccall "helpers.h initializeGEOSWithHandlers"
    c_initializeGEOSWithHandlers :: IO GEOSContextHandle_t
foreign import ccall "helpers.h uninitializeGEOS"
    c_uninitializeGEOS :: GEOSContextHandle_t -> IO ()
foreign import ccall "helpers.h getNoticeMessage"
    c_getNoticeMessage :: IO CString
foreign import ccall "helpers.h getErrorMessage"
    c_getErrorMessage :: IO CString

foreign import ccall "GEOSGeom_destroy_r"
    c_GEOSGeom_destroy_r :: GEOSContextHandle_t -> GEOSGeometryPtr -> IO ()

foreign import ccall "GEOSWKTReader_create_r"
    c_GEOSWKTReader_create_r :: GEOSContextHandle_t -> IO GEOSWKTReaderPtr
foreign import ccall "GEOSWKTReader_destroy_r"
    c_GEOSWKTReader_destroy_r :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> IO ()

-- Should probably be in IO
-- Open question: How to manage lifetime of returned GEOSGeometryPtr?
-- I would like to use ForeignPtr to wrap it so that the native pointer
-- is automatically deleted using GEOSGeom_destroy_t. Unfortunately,
-- this function requires a GEOSContextHandle_t, so I'm not sure how to
-- wrap it yet. An alternative would be to provide a wrapping "bracket"-
-- style function, but this would be ugly.
foreign import ccall "GEOSWKTReader_read_r"
    c_GEOSWKTReader_read_r ::
    GEOSContextHandle_t ->
    GEOSWKTReaderPtr ->
    CString ->
    GEOSGeometryPtr
readGeometry :: GEOSContextHandle_t -> GEOSWKTReaderPtr -> CString -> Maybe GEOSGeometryPtr
readGeometry h r wkt =
    let g = c_GEOSWKTReader_read_r h r wkt
    in if isNull g then Nothing else Just g

foreign import ccall "GEOSWKTWriter_create_r"
    c_GEOSWKTWriter_create_r :: GEOSContextHandle_t -> IO GEOSWKTWriterPtr
foreign import ccall "GEOSWKTWriter_destroy_r"
    c_GEOSWKTWriter_destroy_r :: GEOSContextHandle_t -> GEOSWKTWriterPtr -> IO ()
foreign import ccall "GEOSWKTWriter_write_r"
    c_GEOSWKTWriter_write_r :: GEOSContextHandle_t -> GEOSWKTWriterPtr -> GEOSGeometryPtr -> IO CString

-- Should this be considered pure?
foreign import ccall "GEOSIntersection_r"
    c_GEOSIntersection_r ::
    GEOSContextHandle_t ->
    GEOSGeometryPtr ->
    GEOSGeometryPtr ->
    GEOSGeometryPtr
intersection :: GEOSContextHandle_t -> GEOSGeometryPtr -> GEOSGeometryPtr -> Maybe GEOSGeometryPtr
intersection h g0 g1 =
    let g2 = c_GEOSIntersection_r h g0 g1
    in if isNull g2 then Nothing else Just g2

isNull :: GEOSGeometryPtr -> Bool
isNull (GEOSGeometryPtr p) = p == nullPtr

withGEOS :: (GEOSContextHandle_t -> IO a) -> IO a
withGEOS = bracket c_initializeGEOSWithHandlers c_uninitializeGEOS

withWKTReader :: GEOSContextHandle_t -> (GEOSWKTReaderPtr -> IO a) -> IO a
withWKTReader h = bracket (c_GEOSWKTReader_create_r h) (c_GEOSWKTReader_destroy_r h)

withWKTWriter :: GEOSContextHandle_t -> (GEOSWKTWriterPtr -> IO a) -> IO a
withWKTWriter h = bracket (c_GEOSWKTWriter_create_r h) (c_GEOSWKTWriter_destroy_r h)

main :: IO ()
main = do
    let cs = c_GEOSversion
    s <- peekCString cs
    putStrLn s

    wkt0 <- newCString "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
    wkt1 <- newCString "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"

    void $ withGEOS $ \h -> do
        (g0, g1) <- withWKTReader h $ \reader -> do
            let !(Just g0) = readGeometry h reader wkt0
                !(Just g1) = readGeometry h reader wkt1
            return (g0, g1)
        let (Just g2) = intersection h g0 g1
        print g0
        print g1
        print g2
        withWKTWriter h $ \writer -> do
            g2cs <- c_GEOSWKTWriter_write_r h writer g2
            g2Str <- peekCString g2cs
            print g2Str
        c_GEOSGeom_destroy_r h g2
        c_GEOSGeom_destroy_r h g1
        c_GEOSGeom_destroy_r h g0
    putStrLn "Done"
