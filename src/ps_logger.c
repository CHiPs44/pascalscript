/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>

#include "ps_logger.h"
#include "ps_memory.h"

ps_logger *ps_logger_alloc(FILE *file, ps_debug_level debug_level)
{
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

void ps_log(ps_logger *logger, ps_debug_level debug_level, const char *message)
{
    assert(NULL != logger);
    if (logger->debug_level >= debug_level && message != NULL)
    {
        fprintf(logger->file, "%s\n", message);
    }
}
