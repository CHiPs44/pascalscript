/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

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

void *ps_memory_malloc(size_t size)
{
    mallocations += 1;
    mallocated += size;
    return malloc(size);
}

void *ps_memory_calloc(size_t count, size_t size)
{
    callocations += 1;
    callocated += count * size;
    return calloc(count, size);
}

void *ps_memory_realloc(void *ptr, size_t size)
{
    reallocations += 1;
    reallocated += size;
    return realloc(ptr, size);
}

void ps_memory_free(void *ptr)
{
    frees += 1;
    free(ptr);
}

void ps_memory_debug(FILE *output)
{
    size_t total_allocations = mallocations + callocations + reallocations;
    size_t total_allocated = mallocated + callocated + reallocated;
    if (output == NULL)
        output = stderr;
    fprintf(output, "        | calls    | bytes    |\n");
    fprintf(output, "        |----------|----------|\n");
    fprintf(output, "malloc  | %8u | %8u |\n", mallocations, mallocated);
    fprintf(output, "calloc  | %8u | %8u |\n", callocations, callocated);
    fprintf(output, "realloc | %8u | %8u |\n", reallocations, reallocated);
    fprintf(output, "        |----------|----------|\n");
    fprintf(output, "totals  | %8u | %8u |\n", total_allocations, total_allocated);
    fprintf(output, "        |----------|----------|\n");
    fprintf(output, "free    | %8u |        - |\n", frees);
}
