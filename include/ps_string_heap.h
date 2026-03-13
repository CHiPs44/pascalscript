/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_STRING_HEAP_H
#define _PS_STRING_HEAP_H

#include <stdio.h>

#include "ps_string.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_STRING_HEAP_SIZE
#define PS_STRING_HEAP_SIZE (16)
#endif

#ifndef PS_STRING_HEAP_MORE
#define PS_STRING_HEAP_MORE (16)
#endif

    /**
     * @brief String heap to make string constants unique instead of duplicating them.
     *        It saves a lot of memory for string constants used in loops.
     */
    typedef struct s_ps_string_heap
    {
        size_t size;      /** @brief allocated count            */
        size_t used;      /** @brief currently used count       */
        size_t more;      /** @brief amount added for each grow */
        ps_string **data; /** @brief the strings thmeselves     */
    } ps_string_heap;

#define PS_STRING_HEAP_SIZEOF sizeof(ps_string_heap)

    ps_string_heap *ps_string_heap_alloc(size_t size, size_t more);
    ps_string_heap *ps_string_heap_free(ps_string_heap *heap);
    ps_string *ps_string_heap_create(ps_string_heap *heap, const char *z);
    bool ps_string_heap_grow(ps_string_heap *heap);
    bool ps_string_heap_free_string(ps_string_heap *heap, ps_string *s);
    void ps_string_heap_dump(const ps_string_heap *heap, FILE *f);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_HEAP_H */
