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

#define PS_MEMORY_SYSTEM 0
#define PS_MEMORY_BUFFER 1
#define PS_MEMORY_ENVIRONMENT 2
#define PS_MEMORY_EXECUTABLE 3
#define PS_MEMORY_INTERPRETER 4
#define PS_MEMORY_LEXER 5
#define PS_MEMORY_PARSER 6
#define PS_MEMORY_SIGNATURE 7
#define PS_MEMORY_STRING 8
#define PS_MEMORY_SYMBOL 9
#define PS_MEMORY_TYPE 10
#define PS_MEMORY_VALUE 11
#define PS_MEMORY_CLASS_COUNT 12

    void *ps_memory_malloc(int memory_class, size_t size);
    void *ps_memory_calloc(int memory_class, size_t count, size_t size);
    void *ps_memory_realloc(int memory_class, void *ptr, size_t size);
    void ps_memory_free(int memory_class, void *ptr);
    void ps_memory_debug(FILE *output);

#ifdef __cplusplus
}
#endif

#endif /* _PS_MEMORY_H */
