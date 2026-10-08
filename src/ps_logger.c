/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>
#include <time.h>

#include "ps_logger.h"
#include "ps_memory.h"

ps_logger *ps_logger_alloc(FILE *file, ps_debug_level debug_level)
{
    assert(NULL != file);

    ps_logger *logger = ps_memory_malloc(PS_MEMORY_SYSTEM, sizeof(ps_logger));
    if (logger == NULL)
        return NULL;
    logger->file = file;
    logger->debug_level = debug_level;

    return logger;
}

ps_logger *ps_logger_free(ps_logger *logger)
{
    if (logger != NULL)
        ps_memory_free(PS_MEMORY_SYSTEM, logger);

    return NULL;
}

static char *ps_log_get_timestamp()
{
    static char timestamp[32];
    time_t now;
    struct tm tm_info;

    time(&now);
    localtime_r(&now, &tm_info);
    strftime(timestamp, sizeof(timestamp), "%Y-%m-%d %H:%M:%S", &tm_info);

    return timestamp;
}

void ps_log(ps_logger *logger, ps_debug_level debug_level, const char *message)
{
    assert(NULL != logger);
    assert(NULL != message);

    if (logger->debug_level >= debug_level && message != NULL)
    {
        fprintf(logger->file, "[%s] %s\n", ps_log_get_timestamp(), message);
    }
}
