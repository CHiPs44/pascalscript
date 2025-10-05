/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_MEMORY_H
#define _PS_MEMORY_H

#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif

    void *ps_memory_malloc(size_t size);
    void *ps_memory_calloc(size_t count, size_t size);
    void *ps_memory_realloc(void *ptr, size_t size);
    void ps_memory_free(void *ptr);
    void ps_memory_debug(FILE *output);

#ifdef __cplusplus
}
#endif

#endif /* _PS_MEMORY_H */
