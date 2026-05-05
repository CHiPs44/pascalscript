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

typedef enum
{
    PS_MEMORY_SYSTEM,
    PS_MEMORY_BUFFER,
    PS_MEMORY_ENVIRONMENT,
    PS_MEMORY_EXECUTABLE,
    PS_MEMORY_INTERPRETER,
    PS_MEMORY_LEXER,
    PS_MEMORY_PARSER,
    PS_MEMORY_SIGNATURE,
    PS_MEMORY_STRING,
    PS_MEMORY_SYMBOL,
    PS_MEMORY_TYPE,
    PS_MEMORY_VALUE,
    PS_MEMORY_AST,
    PS_MEMORY_CLASS_COUNT
} ps_memory_class;

    void *ps_memory_malloc(ps_memory_class memory_class, size_t size);
    void *ps_memory_calloc(ps_memory_class memory_class, size_t count, size_t size);
    void *ps_memory_realloc(ps_memory_class memory_class, void *ptr, size_t size);
    void ps_memory_free(ps_memory_class memory_class, void *ptr);
    void ps_memory_debug(FILE *output);

#ifdef __cplusplus
}
#endif

#endif /* _PS_MEMORY_H */
