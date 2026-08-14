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

    // Forward reference
    typedef struct s_ps_ast_block ps_ast_block;

    /** @brief Stack frame holding variables & parameters */
    typedef struct s_ps_frame
    {
        ps_ast_block *block;  /** @brief Block with symbols, variabless, parameters & lexical parent   */
        ps_value_data data[]; /** @brief Variable type is in symbol, index is handle from symbol (VLA) */
    } ps_frame;

    /** @brief Stack itself */
    typedef struct s_ps_stack
    {
        size_t size;        /** @brief Count of frames in stack */
        size_t used;        /** @brief Count of frames used     */
        size_t sp;          /** @brief Stack pointer            */
        ps_frame *frames[]; /** @brief Frames (VLA)             */
    } ps_stack;

#define PS_FRAME_SIZE sizeof(ps_frame)
#define PS_STACK_SIZE sizeof(ps_stack)

    /**
     * @brief Allocate a new frame for block variables & parameters with parent frame
     * @param block The AST block
     * @return NULL if allocation failed
     */
    ps_frame *ps_frame_alloc(ps_ast_block *block);

    /**
     * @brief Free a frame
     * @param frame The frame to free
     * @return NULL
     */
    ps_frame *ps_frame_free(ps_frame *frame);

    /**
     * @brief Allocate a new stack for size frames
     * @param size Maximum number of frames in stack
     * @return NULL if allocation failed
     */
    ps_stack *ps_stack_alloc(size_t size);

    /**
     * @brief Free a stack
     * @param stack The stack to free
     * @return NULL
     */
    ps_stack *ps_stack_free(ps_stack *stack);

    /**
     * @brief Push a frame on the stack
     * @param stack The stack
     * @param frame The frame to push
     * @return NULL if stack is full else frame
     */
    ps_frame *ps_stack_push(ps_stack *stack, ps_frame *frame);

    /**
     * @brief Pop a frame
     * @param stack The stack
     * @return NULL if stack is empty else frame
     * @note Frame is not freed, caller must free it
     */
    ps_frame *ps_stack_pop(ps_stack *stack);

    /**
     * @brief Get the top frame
     * @param stack The stack
     * @return NULL if stack is empty else top frame
     */
    ps_frame *ps_stack_top(const ps_stack *stack);

    /**
     * @brief Check if stack is empty
     * @param stack The stack
     * @return true if stack is empty, false otherwise
     */
    bool ps_stack_is_empty(const ps_stack *stack);

    /**
     * @brief Check if stack is full
     * @param stack The stack
     * @return true if stack is full, false otherwise
     */
    bool ps_stack_is_full(const ps_stack *stack);

    /**
     * @brief Find the top frame for a block
     * @param stack The stack
     * @param block The block to search for
     * @return The frame for the block or NULL if not found
     */
    ps_frame *ps_stack_find_frame_for_block(ps_stack *stack, ps_ast_block *block);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STACK_H */
