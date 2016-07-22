#pragma once

#include "geos_c.h"

GEOSContextHandle_t initializeGEOSWithHandlers();
const char* getNoticeMessage();
const char* getErrorMessage();
