/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_STRING_STACK_H
#define _PS_STRING_STACK_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "ps_string.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Stack of string pointers */
    typedef struct s_ps_string_stack
    {
        size_t size;          /** @brief Maximum number of strings */
        size_t sp;            /** @brief Stack pointer */
        ps_string *strings[]; /** @brief Strings (VLA) */
    } ps_string_stack;

#define PS_STRING_STACK_SIZE sizeof(ps_string_stack)

    /**
     * @brief Allocate a string stack
     * @param size Maximum number of strings in stack
     * @return NULL if allocation failed
     */
    ps_string_stack *ps_string_stack_alloc(size_t size);

    /**
     * @brief Free a string stack with an option to freei the strings it contains
     * @param stack The string stack to free
     * @param free_strings
     * @return NULL
     */
    ps_string_stack *ps_string_stack_free(ps_string_stack *stack, bool free_strings);

    /**
     * @brief Push a string pointer onto the stack
     * @return NULL if the stack is full or an argument is invalid, else string
     */
    ps_string *ps_string_stack_push(ps_string_stack *stack, ps_string *string);

    /**
     * @brief Pop a string pointer from the stack
     * @return NULL if the stack is empty, else the popped string
     */
    ps_string *ps_string_stack_pop(ps_string_stack *stack);

    /**
     * @brief Get the top string pointer
     * @return NULL if the stack is empty, else the top string
     */
    ps_string *ps_string_stack_top(const ps_string_stack *stack);

    bool ps_string_stack_is_empty(const ps_string_stack *stack);
    bool ps_string_stack_is_full(const ps_string_stack *stack);
    void ps_string_stack_dump(FILE *output, const ps_string_stack *stack);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_STACK_H */
