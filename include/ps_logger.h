/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_LOGGER_H
#define _PS_LOGGER_H

#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum e_ps_debug_level
    {
        PS_DEBUG_FATAL = 0,
        PS_DEBUG_CRITICAL,
        PS_DEBUG_ERROR,
        PS_DEBUG_WARNING,
        PS_DEBUG_INFO,
        PS_DEBUG_DEBUG,
        PS_DEBUG_TRACE,
        PS_DEBUG_VERBOSE,
    } __attribute__((__packed__)) ps_debug_level;

    typedef struct s_ps_logger
    {
        FILE *file;
        ps_debug_level debug_level;
    } ps_logger;

    ps_logger *ps_logger_alloc(FILE *file, ps_debug_level debug_level);
    ps_logger *ps_logger_free(ps_logger *logger);
    void ps_log(ps_logger *logger, ps_debug_level debug_level, const char *message);

#ifdef __cplusplus
}
#endif

#endif /* _PS_LOGGER_H */
