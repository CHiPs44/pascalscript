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

    /** @brief Stack frame */
    typedef struct s_ps_frame
    {
        ps_ast_block *block;       /** @brief Block with symbols, vars, params & lexical parent (static)   */
        struct s_ps_frame *parent; /** @brief Parent frame (dynamic), NULL if no parent                    */
        ps_value_data data[];      /** @brief Variable type is in symbol, index is handle from symbol, too */
    } ps_frame;

    /** @brief Stack itself */
    typedef struct s_ps_stack
    {
        size_t size;        /** @brief Count of frames in stack */
        size_t used;        /** @brief Count of frames used     */
        size_t sp;          /** @brief Stack pointer            */
        ps_frame *frames[]; /** @brief Frames                   */
    } ps_stack;

#define PS_FRAME_SIZE sizeof(ps_frame)
#define PS_STACK_SIZE sizeof(ps_stack)

    /** @brief Allocate a new frame for block variables & parameters with parent frame */
    /** @return NULL if allocation failed */
    ps_frame *ps_frame_alloc(ps_ast_block *block, ps_frame *parent);

    /** @brief Free a frame */
    /** @return NULL */
    ps_frame *ps_frame_free(ps_frame *frame);

    /** @brief Allocate a new stack for size frames */
    /** @return NULL if allocation failed */
    ps_stack *ps_stack_alloc(size_t size);

    /** @brief Free a stack */
    /** @return NULL */
    ps_stack *ps_stack_free(ps_stack *stack);

    /** @brief Push a frame on the stack */
    /** @return NULL if stack is full else frame */
    ps_frame *ps_stack_push(ps_stack *stack, ps_frame *frame);

    /** @brief Pop a frame */
    /** @return NULL if stack is empty else frame */
    ps_frame *ps_stack_pop(ps_stack *stack);

    /** @brief Get the top frame */
    /** @return NULL if stack is empty */
    ps_frame *ps_stack_top(const ps_stack *stack);

    /** @brief Check if stack is empty */
    bool ps_stack_is_empty(const ps_stack *stack);

    /** @brief Check if stack is full */
    bool ps_stack_is_full(const ps_stack *stack);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STACK_H */
