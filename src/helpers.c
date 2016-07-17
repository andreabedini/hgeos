#include "helpers.h"
#include <assert.h>
#include <geos_c.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//#define TRACE(message) printf(message "\n")
#define TRACE(message) do {} while (0)

__thread char g_noticeMessage[256];
static const size_t s_noticeMessageLen = sizeof(g_noticeMessage) / sizeof(g_noticeMessage[0]);
__thread char g_errorMessage[256];
static const size_t s_errorMessageLen = sizeof(g_errorMessage) / sizeof(g_errorMessage[0]);

static void noticeHandler(const char* format, ...)
{
    va_list args;
    va_start(args, format);
    vsnprintf(g_noticeMessage, s_noticeMessageLen, format, args);
    va_end(args);
}

static void errorHandler(const char* format, ...)
{
    va_list args;
    va_start(args, format);
    vsnprintf(g_errorMessage, s_errorMessageLen, format, args);
    va_end(args);
}

GEOSContextHandle_t initializeGEOSWithHandlers()
{
    return initGEOS_r(noticeHandler, errorHandler);
}

void uninitializeGEOS(GEOSContextHandle_t handle)
{
    finishGEOS_r(handle);
}

const char* getNoticeMessage()
{
    return g_noticeMessage;
}

const char* getErrorMessage()
{
    return g_errorMessage;
}

//////////

struct String;

struct Context
{
    GEOSContextHandle_t handle;
    struct Reader* headReader;
    struct Geometry* headGeometry;
    struct Writer* headWriter;
    struct String* headString;
};

struct Reader
{
    struct Context* ctx;
    GEOSWKTReader* handle;
    struct Reader* tail;
};

struct Geometry
{
    struct Context* ctx;
    GEOSGeometry* handle;
    struct Geometry* tail;
};

struct Writer
{
    struct Context* ctx;
    GEOSWKTWriter* handle;
    struct Writer* tail;
};

struct String
{
    struct Context* ctx;
    char* handle;
    struct String* tail;
};

static void freeReaders(struct Reader* reader)
{
    if (reader)
    {
        TRACE("GEOSWKTReader_destroy_r");
        GEOSWKTReader_destroy_r(reader->ctx->handle, reader->handle);
        freeReaders(reader->tail);
        free(reader);
    }
}

static void freeGeometries(struct Geometry* geometry)
{
    if (geometry)
    {
        TRACE("GEOSGeom_destroy_r");
        GEOSGeom_destroy_r(geometry->ctx->handle, geometry->handle);
        freeGeometries(geometry->tail);
        free(geometry);
    }
}

static void freeWriters(struct Writer* writer)
{
    if (writer)
    {
        TRACE("GEOSWKTWriter_destroy_r");
        GEOSWKTWriter_destroy_r(writer->ctx->handle, writer->handle);
        freeWriters(writer->tail);
        free(writer);
    }
}

static void freeStrings(struct String* string)
{
    if (string)
    {
        TRACE("GEOSFree_r");
        GEOSFree_r(string->ctx->handle, string->handle);
        freeStrings(string->tail);
        free(string);
    }
}

ContextPtr createContext()
{
    struct Context* ctx = (struct Context*)malloc(sizeof(struct Context));
    assert(ctx);
    memset(ctx, 0, sizeof(struct Context));

    TRACE("initGEOS_r");
    ctx->handle = initGEOS_r(noticeHandler, errorHandler);
    assert(ctx->handle);

    return ctx;
}

void contextDestroy(ContextPtr ctx)
{
    assert(ctx);

    freeGeometries(ctx->headGeometry);
    freeWriters(ctx->headWriter);
    freeReaders(ctx->headReader);
    TRACE("finishGEOS_r");
    finishGEOS_r(ctx->handle);
    free(ctx);
}

GEOSContextHandle_t contextGetHandle(ContextPtr ctx)
{
    assert(ctx);

    return ctx->handle;
}

ReaderPtr contextCreateReader(ContextPtr ctx)
{
    assert(ctx);

    struct Reader* reader = (struct Reader*)malloc(sizeof(struct Reader));
    assert(reader);
    memset(reader, 0, sizeof(struct Reader));

    reader->ctx = ctx;
    TRACE("GEOSWKTReader_create_r");
    reader->handle = GEOSWKTReader_create_r(ctx->handle);
    assert(reader->handle);
    reader->tail = ctx->headReader;
    ctx->headReader = reader;

    return reader;
}

GeometryPtr readerRead(ReaderPtr reader, const char* wkt)
{
    assert(reader);
    assert(wkt);

    struct Geometry* geometry = (struct Geometry*)malloc(sizeof(struct Geometry));
    assert(geometry);
    memset(geometry, 0, sizeof(struct Geometry));

    geometry->ctx = reader->ctx;
    TRACE("GEOSWKTReader_read_r");
    geometry->handle = GEOSWKTReader_read_r(reader->ctx->handle, reader->handle, wkt);
    assert(geometry->handle);
    geometry->tail = reader->ctx->headGeometry;
    reader->ctx->headGeometry = geometry;

    return geometry;
}

WriterPtr contextCreateWriter(ContextPtr ctx)
{
    assert(ctx);

    struct Writer* writer = (struct Writer*)malloc(sizeof(struct Writer));
    assert(writer);
    memset(writer, 0, sizeof(struct Writer));

    writer->ctx = ctx;
    TRACE("GEOSWKTWriter_create_r");
    writer->handle = GEOSWKTWriter_create_r(ctx->handle);
    assert(writer->handle);
    writer->tail = ctx->headWriter;
    ctx->headWriter = writer;

    return writer;
}

char* writerWrite(WriterPtr writer, GeometryPtr geometry)
{
    assert(writer);
    assert(geometry);

    struct String* string = (struct String*)malloc(sizeof(struct String));
    assert(string);
    memset(string, 0, sizeof(struct String));

    string->ctx = writer->ctx;
    TRACE("GEOSWKTWriter_write_r");
    string->handle = GEOSWKTWriter_write_r(writer->ctx->handle, writer->handle, geometry->handle);
    assert(string->handle);
    string->tail = writer->ctx->headString;
    writer->ctx->headString = string;

    return string->handle;
}
