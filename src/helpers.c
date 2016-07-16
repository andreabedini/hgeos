#include "helpers.h"
#include <geos_c.h>
#include <stdarg.h>
#include <stdio.h>

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
