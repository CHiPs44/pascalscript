/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <malloc.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_memory.h"

size_t mallocations = 0;
size_t callocations = 0;
size_t reallocations = 0;
size_t mallocated = 0;
size_t callocated = 0;
size_t reallocated = 0;
size_t frees = 0;

bool ps_memory_debug_enabled = false;

/**
 * @brief Allocate and count size allocated
 * @return pointer to allocated memory or NULL on failure (errno=ENOMEM)
 */
void *ps_memory_malloc(size_t size)
{
    mallocations += 1;
    if (ps_memory_debug_enabled)
        fprintf(stderr, "%04zu MALLOC %8zu bytes at ?\n", mallocations, size);
    void *ptr = malloc(size);
    if (ptr != NULL)
        mallocated += size;
    if (ps_memory_debug_enabled)
    {
        // FILE *f = fopen("/tmp/malloc_info.log", "w");
        // malloc_info(0, f);
        // fclose(f);
        size_t size2 = ptr == NULL ? 0 : malloc_usable_size(ptr);
        fprintf(stderr, "%04zu MALLOC %8zu bytes at %p, size %8zu\n", mallocations, size, ptr, size2);
    }
    return ptr;
}

/**
 * @brief Allocate count elements of size bytes each, all initialized to 0.
 * @return pointer to allocated memory or NULL on failure (errno=ENOMEM)
 */
void *ps_memory_calloc(size_t count, size_t size)
{
    callocations += 1;
    if (size == 0 || count == 0)
    {
        if (ps_memory_debug_enabled)
            fprintf(stderr, "WARNING: CALLOC with count=%zu size=%zu\n", count, size);
        return NULL;
    }
    callocated += count * size;
    void *ptr = calloc(count, size);
    if (ps_memory_debug_enabled)
    {
        size_t size2 = ptr == NULL ? 0 : malloc_usable_size(ptr);
        fprintf(stderr, "%04zu CALLOC %8zu bytes at %p, size %8zu\n", callocations, count * size, ptr, size2);
    }
    return ptr;
}

/**
 * @brief Change the size of the memory block pointed to by ptr to size bytes.
 * @return pointer to reallocated memory or NULL on failure (errno=ENOMEM)
 */
void *ps_memory_realloc(void *ptr, size_t size)
{
    char old[16] = {0};
    size_t size1 = 0;
    size_t size2 = 0;
    void *new = NULL;

    reallocations += 1;
    reallocated += size;
    if (ps_memory_debug_enabled)
    {
        // avoid use after free in debug output
        size1 = malloc_usable_size(old);
        snprintf(old, sizeof(old) - 1, "%p", ptr);
        new = realloc(ptr, size);
        size2 = malloc_usable_size(new);
        fprintf(stderr, "%04zu REALLOC %8zu bytes at %s => %p, size %8zu => %8zu\n", reallocations, size, old, new,
                size1, size2);
    }
    else
    {
        new = realloc(ptr, size);
    }

    return new;
}

/**
 * @brief Deallocate memory previously allocated by malloc, calloc or realloc.
 */
void ps_memory_free(void *ptr)
{
    frees += 1;
    free(ptr);
}

/**
 * @brief Print memory allocation statistics to the specified output stream.
 * If output is NULL, it defaults to stderr.
 */
void ps_memory_debug(FILE *output)
{
    size_t total_allocations = mallocations + callocations + reallocations;
    size_t total_allocated = mallocated + callocated + reallocated;
    if (output == NULL)
        output = stderr;
    fprintf(output, "        | calls    | bytes    |\n");
    fprintf(output, "        |----------|----------|\n");
    fprintf(output, "malloc  | %8zu | %8zu |\n", mallocations, mallocated);
    fprintf(output, "calloc  | %8zu | %8zu |\n", callocations, callocated);
    fprintf(output, "realloc | %8zu | %8zu |\n", reallocations, reallocated);
    fprintf(output, "        |----------|----------|\n");
    fprintf(output, "totals  | %8zu | %8zu |\n", total_allocations, total_allocated);
    fprintf(output, "        |----------|----------|\n");
    fprintf(output, "free    | %8zu |        - |\n", frees);
}
