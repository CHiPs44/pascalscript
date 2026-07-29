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
        PS_MEMORY_STACK,
        PS_MEMORY_EXECUTABLE,
        PS_MEMORY_COMPILER,
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

    /**
     * @brief Allocate memory.
     * @param memory_class The memory class to allocate memory for.
     * @param size The size of the memory to allocate.
     * @return A pointer to the allocated memory, or NULL if the allocation failed.
     */
    void *ps_memory_malloc(ps_memory_class memory_class, size_t size);

    /**
     * @brief Allocate memory and initialize it to zero.
     * @param memory_class The memory class to allocate memory for.
     * @param count The number of elements to allocate.
     * @param size The size of each element.
     * @return A pointer to the allocated memory, or NULL if the allocation failed.
     */
    void *ps_memory_calloc(ps_memory_class memory_class, size_t count, size_t size);

    /**
     * @brief Reallocate memory.
     * @param memory_class The memory class to reallocate memory for.
     * @param ptr The pointer to the memory to reallocate.
     * @param size The new size of the memory.
     * @return A pointer to the reallocated memory, or NULL if the reallocation failed.
     */
    void *ps_memory_realloc(ps_memory_class memory_class, void *ptr, size_t size);

    /**
     * @brief Deallocate memory previously allocated by malloc, calloc or realloc.
     */
    void ps_memory_free(ps_memory_class memory_class, void *ptr);

    /**
     * @brief Print memory allocation statistics to the specified output stream.
     * @param output The output stream to print to, if NULL, defaults to stderr.
     */
    void ps_memory_debug(FILE *output);

#ifdef __cplusplus
}
#endif

#endif /* _PS_MEMORY_H */
