/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_STRING_HEAP_H
#define _PS_STRING_HEAP_H

#include <stdbool.h>

#include "ps_system_types.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_STRING_HEAP_SIZE
#define PS_STRING_HEAP_SIZE 256
#endif

#define PS_STRING_HEAP_ERROR_OVERFLOW -1

    typedef struct s_ps_string_xxx
    {
        ps_string_len max;
        ps_string_len len;
        ps_char *ptr; // => NULL = free
    } ps_string_heap_cell;

    typedef struct s_ps_string_heap
    {
        int count;
        ps_string_heap_cell *strings[PS_STRING_HEAP_SIZE];
    } ps_string_heap;

    // clang-format off
    ps_string_num  ps_string_heap_find_free(ps_string_heap *heap);
    void           ps_string_heap_free(ps_string_heap *heap, ps_string_num num);
    void           ps_string_heap_set (ps_string_heap *heap, ps_string_num num, ps_string_len max, char *str);
    ps_string_heap_cell *ps_string_heap_get (ps_string_heap *heap, ps_string_num num);
    // clang-format on

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_HEAP_H */
