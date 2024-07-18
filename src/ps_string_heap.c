/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_value.h"
#include "ps_string_heap.h"

/**
 * @brief Initialize heap
 *
 * @param heap
 */
void ps_string_heap_init(ps_string_heap *heap)
{
    heap->count = 0;
    for (int i = 0; i < PS_STRING_HEAP_SIZE; i++)
    {
        heap->strings[i]->max = 0;
        heap->strings[i]->len = 0;
        heap->strings[i]->ptr = NULL;
    }
}

/**
 * @brief Get actual heap size
 */
int ps_string_heap_size(ps_string_heap *heap)
{
    return heap->count;
}

/**
 * @brief Check if heap is full
 *
 * @param Stack
 * @return true if heap is full
 */
extern bool ps_string_heap_full(ps_string_heap *heap)
{
    return ps_string_heap_size(heap) >= PS_STRING_HEAP_SIZE;
}

void ps_string_heap_dump(ps_string_heap *heap, char *title)
{
    ps_string_heap_cell *string_xxx;
    fprintf(stderr, "*** String heap %s (%d)%s ***\n",
            title,
            ps_string_heap_size(heap),
            ps_string_heap_full(heap) ? " (FULL)" : "");
    //               0        1         2         3         4         5         6         7         8
    //               12345678901234567890123456789012345678901234567890123456789012345678901234567890
    //                                   1234567890123456789012345678901234567890123456789012345678
    fprintf(stderr, "┏━━━━━┳━━━━━┳━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(stderr, "┃  #  ┃ Max ┃ Len ┃ String                                                     ┃\n");
    fprintf(stderr, "┣━━━━━╋━━━━━╋━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < PS_STRING_HEAP_SIZE; i++)
    {
        if (heap->strings[i] != NULL)
        {
            string_xxx = heap->strings[i];
            fprintf(stderr, "┃ %3d ┃ %3d ┃ %3d ┃%-*s┃\n", i, string_xxx->max, string_xxx->len, 58, string_xxx->ptr);
        }
    }
    fprintf(stderr, "┗━━━━━┻━━━━━┻━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
}



/* EOF */
