#pragma once

#include "geos_c.h"

GEOSContextHandle_t initializeGEOSWithHandlers();
void uninitializeGEOS(GEOSContextHandle_t handle);
const char* getNoticeMessage();
const char* getErrorMessage();
