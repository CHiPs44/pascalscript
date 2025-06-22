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
#define PS_STRING_HEAP_SIZE (256)
#endif

    typedef struct s_ps_string_heap
    {
        size_t size;
        size_t used;
        // size_t more;
        ps_string **data;
    } ps_string_heap;
    #define PS_STRING_HEAP_SIZEOF sizeof(ps_string_heap)

    ps_string_heap *ps_string_heap_init(size_t size);
    void ps_string_heap_done(ps_string_heap *heap);
    ps_string *ps_string_heap_create(ps_string_heap *heap, char *z);
    // bool ps_string_heap_free(ps_string_heap *heap, ps_string *s);

    void ps_string_heap_dump(ps_string_heap *heap, FILE *f);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_HEAP_H */
