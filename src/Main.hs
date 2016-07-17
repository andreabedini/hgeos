{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Data.Geocoding.GEOS.Imports

withGEOS :: (GEOSContextHandle_t -> IO a) -> IO a
withGEOS = bracket c_initializeGEOSWithHandlers c_uninitializeGEOS

withWKTReader :: GEOSContextHandle_t -> (GEOSWKTReaderPtr -> IO a) -> IO a
withWKTReader h = bracket (c_GEOSWKTReader_create_r h) (c_GEOSWKTReader_destroy_r h)

withWKTWriter :: GEOSContextHandle_t -> (GEOSWKTWriterPtr -> IO a) -> IO a
withWKTWriter h = bracket (c_GEOSWKTWriter_create_r h) (c_GEOSWKTWriter_destroy_r h)

wrap :: GEOSGeometryPtr -> Maybe GEOSGeometryPtr
wrap g@(GEOSGeometryPtr p) = if p == nullPtr then Nothing else Just g

-- TODO: Just show raw pointer value for now!
instance Show GEOSGeometryPtr where
    show (GEOSGeometryPtr p) = show p

-- TODO: Looks kinda state-monady
data Context = Context
    { handle :: GEOSContextHandle_t
    , geometries :: [GEOSGeometryPtr]}

readGeometry :: Context -> GEOSWKTReaderPtr -> CString -> IO (Maybe GEOSGeometryPtr, Context)
readGeometry ctx@(Context h _) reader wkt = do
    g@(GEOSGeometryPtr p) <- c_GEOSWKTReader_read_r h reader wkt
    return $
        if p == nullPtr
        then (Nothing, ctx)
        else (Just g, ctx { geometries = geometries ctx ++ [g] })

rawApiDemo :: IO ()
rawApiDemo = do
    wkt0 <- newCString "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
    wkt1 <- newCString "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"

    withGEOS $ \h -> do
        (g0, g1) <- withWKTReader h $ \reader -> do
            !(Just g0) <- wrap <$> c_GEOSWKTReader_read_r h reader wkt0
            !(Just g1) <- wrap <$> c_GEOSWKTReader_read_r h reader wkt1
            return (g0, g1)
        !(Just g2) <- wrap <$> c_GEOSIntersection_r h g0 g1
        print g0
        print g1
        print g2
        withWKTWriter h $ \writer -> do
            g2cs <- c_GEOSWKTWriter_write_r h writer g2
            g2Str <- peekCString g2cs
            print g2Str
            c_GEOSFree_r_CChar h g2cs -- TODO: Use bracket
        c_GEOSGeom_destroy_r h g2 -- TODO: Use bracket
        c_GEOSGeom_destroy_r h g1 -- TODO: Use bracket
        c_GEOSGeom_destroy_r h g0 -- TODO: Use bracket
        putStrLn "rawApiDemo done"

mkContext :: IO Context
mkContext = c_initializeGEOSWithHandlers >>= \h -> return $ Context h []

destroyContext :: Context -> IO ()
destroyContext (Context h _) = c_uninitializeGEOS h

newtype ContextPtr = ContextPtr (Ptr ContextPtr)
newtype ReaderPtr = ReaderPtr (Ptr ReaderPtr)
newtype WriterPtr = WriterPtr (Ptr WriterPtr)
newtype GeometryPtr = GeometryPtr (Ptr GeometryPtr)

foreign import ccall "helpers.h createContext"
    c_createContext :: IO ContextPtr
foreign import ccall "helpers.h contextDestroy"
    c_contextDestroy :: ContextPtr -> IO ()
--foreign import ccall "helpers.h contextGetHandle"
--    c_contextGetHandle :: ContextPtr -> IO GEOSContextHandle_t
foreign import ccall "helpers.h contextCreateReader"
    c_contextCreateReader :: ContextPtr -> IO ReaderPtr
foreign import ccall "helpers.h readerRead"
    c_readerRead :: ReaderPtr -> CString -> IO GeometryPtr
foreign import ccall "helpers.h contextCreateWriter"
    c_contextCreateWriter :: ContextPtr -> IO WriterPtr
foreign import ccall "helpers.h writerWrite"
    c_writerWrite :: WriterPtr -> GeometryPtr -> IO CString
foreign import ccall "helpers.h contextIntersection"
    c_contextIntersection :: GeometryPtr -> GeometryPtr -> IO GeometryPtr

higherLevelApiDemo :: IO ()
higherLevelApiDemo = do
    wkt0 <- newCString "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
    wkt1 <- newCString "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
    bracket c_createContext c_contextDestroy $ \ctx -> do
        reader <- c_contextCreateReader ctx
        g0 <- c_readerRead reader wkt0
        g1 <- c_readerRead reader wkt1
        g2 <- c_contextIntersection g0 g1
        writer <- c_contextCreateWriter ctx
        cs2 <- c_writerWrite writer g2
        s2 <- peekCString cs2
        print s2
        putStrLn "higherLevelApiDemo done"

main :: IO ()
main = do
    s <- peekCString c_GEOSversion
    putStrLn s

    --rawApiDemo
    higherLevelApiDemo
