#pragma once

#include "geos_c.h"

GEOSContextHandle_t initializeGEOSWithHandlers();
void uninitializeGEOS(GEOSContextHandle_t handle);
const char* getNoticeMessage();
const char* getErrorMessage();

//////////

typedef struct Context* ContextPtr;
typedef struct Reader* ReaderPtr;
typedef struct Writer* WriterPtr;
typedef struct Geometry* GeometryPtr;

ContextPtr createContext();
void contextDestroy(ContextPtr ctx);
GEOSContextHandle_t contextGetHandle(ContextPtr ctx);
ReaderPtr contextCreateReader(ContextPtr ctx);
GeometryPtr readerRead(ReaderPtr reader, const char* wkt);
WriterPtr contextCreateWriter(ContextPtr ctx);
char* writerWrite(WriterPtr writer, GeometryPtr geometry);
