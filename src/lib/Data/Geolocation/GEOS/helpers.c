#include <geos_c.h>
#include <stdarg.h>
#include <stdio.h>

#ifdef ENABLE_TRACE
#define TRACE(message) printf(message "\n")
#else
#define TRACE(message) do {} while (0)
#endif

__thread char g_noticeMessage[256];
static const size_t s_noticeMessageLen = sizeof(g_noticeMessage) / sizeof(g_noticeMessage[0]);
__thread char g_errorMessage[256];
static const size_t s_errorMessageLen = sizeof(g_errorMessage) / sizeof(g_errorMessage[0]);

static void noticeHandler(const char* format, ...)
{
    TRACE("noticeHandler");

    va_list args;
    va_start(args, format);
    vsnprintf(g_noticeMessage, s_noticeMessageLen, format, args);
    va_end(args);
}

static void errorHandler(const char* format, ...)
{
    TRACE("errorHandler");

    va_list args;
    va_start(args, format);
    vsnprintf(g_errorMessage, s_errorMessageLen, format, args);
    va_end(args);
}

GEOSContextHandle_t initializeGEOSWithHandlers()
{
    TRACE("initializeGEOSWithHandlers");

    return initGEOS_r(noticeHandler, errorHandler);
}

const char* getNoticeMessage()
{
    TRACE("getNoticeMessage");

    return g_noticeMessage;
}

const char* getErrorMessage()
{
    TRACE("getErrorMessage");

    return g_errorMessage;
}
