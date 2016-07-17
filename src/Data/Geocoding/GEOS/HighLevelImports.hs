module Data.Geocoding.GEOS.HighLevelImports where

import Foreign.C
import Foreign.Ptr

newtype ContextPtr = ContextPtr (Ptr ContextPtr)
newtype ReaderPtr = ReaderPtr (Ptr ReaderPtr)
newtype WriterPtr = WriterPtr (Ptr WriterPtr)
newtype GeometryPtr = GeometryPtr (Ptr GeometryPtr)

foreign import ccall "helpers.h createContext"
    c_createContext :: IO ContextPtr
foreign import ccall "helpers.h contextDestroy"
    c_contextDestroy :: ContextPtr -> IO ()

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
