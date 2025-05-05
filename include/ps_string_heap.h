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
extern 'C'
{
#endif

#define PS_STRING_HEAP_SIZE (32)

    typedef struct s_ps_string_heap
    {
        size_t size;
        size_t used;
        ps_string *data;
    } ps_string_heap;

    ps_string_heap *ps_string_heap_init(ps_string_heap * heap, size_t size);
    void ps_string_heap_done(ps_string_heap * heap);
    ps_string *ps_string_heap_alloc(ps_string_heap * heap, ps_string_len max);
    ps_string *ps_string_heap_free(ps_string_heap * heap, ps_string * s);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_HEAP_H */
