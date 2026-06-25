/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_STACK_H
#define _PS_STACK_H

#include "ps_value_data.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_frame
    {
        size_t size;
        ps_value_data data[];
    } ps_frame;

    /** @brief Stack itself */
    typedef struct s_ps_stack
    {
        size_t size;
        size_t sp;
        ps_frame *frames[];
    } ps_stack;

    ps_frame *ps_frame_alloc(size_t size);
    ps_frame *ps_frame_free(ps_frame *frame);

    ps_stack *ps_stack_alloc(size_t size);
    ps_stack *ps_stack_free(ps_stack *stack);

    ps_frame *ps_stack_push(ps_stack *stack, ps_frame *frame);
    ps_frame *ps_stack_pop(ps_stack *stack);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STACK_H */
