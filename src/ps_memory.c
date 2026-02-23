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

static char *memory_class_names[] = {"SYSTEM", "BUFFER",    "ENVIRONMENT", "EXECUTABLE", "INTERPRETER", "LEXER",
                                     "PARSER", "SIGNATURE", "STRING",      "SYMBOL",     "TYPE",        "VALUE"};

size_t mallocations[PS_MEMORY_CLASS_COUNT] = {0};
size_t callocations[PS_MEMORY_CLASS_COUNT] = {0};
size_t reallocations[PS_MEMORY_CLASS_COUNT] = {0};
size_t mallocated[PS_MEMORY_CLASS_COUNT] = {0};
size_t callocated[PS_MEMORY_CLASS_COUNT] = {0};
size_t reallocated[PS_MEMORY_CLASS_COUNT] = {0};
size_t frees[PS_MEMORY_CLASS_COUNT] = {0};
size_t freed[PS_MEMORY_CLASS_COUNT] = {0};

bool ps_memory_debug_enabled = true;

/**
 * @brief Allocate and count size allocated
 * @return pointer to allocated memory or NULL on failure (errno=ENOMEM)
 */
void *ps_memory_malloc(int memory_class, size_t size)
{
    mallocations[memory_class] += 1;
    void *ptr = malloc(size);
    if (ptr != NULL)
        mallocated[memory_class] += size;
    if (ps_memory_debug_enabled)
    {
        if (false)
        {
            FILE *f = fopen("/tmp/malloc_info.log", "w");
            malloc_info(0, f);
            fclose(f);
        }
        size_t size2 = ptr == NULL ? 0 : malloc_usable_size(ptr);
        fprintf(stderr, "%04zu MALLOC\t%-16s %8zu bytes at %p, size %8zu\n", mallocations[memory_class],
                memory_class_names[memory_class], size, ptr, size2);
    }
    return ptr;
}

/**
 * @brief Allocate count elements of size bytes each, all initialized to 0.
 * @return pointer to allocated memory or NULL on failure (errno=ENOMEM)
 */
void *ps_memory_calloc(int memory_class, size_t count, size_t size)
{
    callocations[memory_class] += 1;
    if (size == 0 || count == 0)
    {
        if (ps_memory_debug_enabled)
            fprintf(stderr, "WARNING: CALLOC with count=%zu size=%zu\n", count, size);
        return NULL;
    }
    callocated[memory_class] += count * size;
    void *ptr = calloc(count, size);
    if (ps_memory_debug_enabled)
    {
        size_t size2 = ptr == NULL ? 0 : malloc_usable_size(ptr);
        fprintf(stderr, "%04zu CALLOC\t%-16s %8zu bytes at %p, size %8zu\n", callocations[memory_class],
                memory_class_names[memory_class], count * size, ptr, size2);
    }
    return ptr;
}

/**
 * @brief Change the size of the memory block pointed to by ptr to size bytes.
 * @return pointer to reallocated memory or NULL on failure (errno=ENOMEM)
 */
void *ps_memory_realloc(int memory_class, void *ptr, size_t size)
{
    void *new = NULL;

    reallocations[memory_class] += 1;
    reallocated[memory_class] += size;
    if (ps_memory_debug_enabled)
    {
        // avoid use after free in debug output
        size_t size1 = malloc_usable_size(ptr);
        char old[16] = {0};
        snprintf(old, sizeof(old) - 1, "%p", ptr);
        new = realloc(ptr, size);
        size_t size2 = malloc_usable_size(new);
        fprintf(stderr, "%04zu REALLOC\t%-16s %8zu bytes at %s => %p, size %8zu => %8zu\n", reallocations[memory_class],
                memory_class_names[memory_class], size, old, new, size1, size2);
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
void ps_memory_free(int memory_class, void *ptr)
{
    frees[memory_class] += 1;
    size_t size = malloc_usable_size(ptr);
    freed[memory_class] += size;
    if (ps_memory_debug_enabled)
    {
        fprintf(stderr, "%04zu FREE\t%-16s %8zu bytes at %p\n", frees[memory_class], memory_class_names[memory_class],
                size, ptr);
    }
    free(ptr);
}

/**
 * @brief Print memory allocation statistics to the specified output stream.
 * If output is NULL, it defaults to stderr.
 */
void ps_memory_debug(FILE *output)
{
    if (output == NULL)
        output = stderr;
    for (int memory_class = 0; memory_class < PS_MEMORY_CLASS_COUNT; memory_class += 1)
    {
        fprintf(output, "========== %s ==========\n", memory_class_names[memory_class]);
        size_t total_allocations =
            mallocations[memory_class] + callocations[memory_class] + reallocations[memory_class];
        size_t total_allocated = mallocated[memory_class] + callocated[memory_class] + reallocated[memory_class];
        fprintf(output, "        | calls    | bytes    |\n");
        fprintf(output, "        |----------|----------|\n");
        fprintf(output, "malloc  | %8zu | %8zu |\n", mallocations[memory_class], mallocated[memory_class]);
        fprintf(output, "calloc  | %8zu | %8zu |\n", callocations[memory_class], callocated[memory_class]);
        fprintf(output, "realloc | %8zu | %8zu |\n", reallocations[memory_class], reallocated[memory_class]);
        fprintf(output, "        |----------|----------|\n");
        fprintf(output, "totals  | %8zu | %8zu |\n", total_allocations, total_allocated);
        fprintf(output, "        |----------|----------|\n");
        fprintf(output, "free    | %8zu |        - |\n", frees[memory_class]);
    }
}
