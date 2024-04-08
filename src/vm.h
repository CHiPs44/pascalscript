/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _VM_H
#define _VM_H

#include "token.h"
#include "symbol_table.h"
#include "symbol_stack.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_LINES
#define MAX_LINES 65536
#endif

#ifndef MAX_COLUMNS
#define MAX_COLUMNS 256
#endif

    typedef struct _vm_t
    {
        char *source;
        size_t length;
        unsigned int line_count;
        char **line_starts;
        uint16_t *line_lengths;
        int current_line;
        int current_column;
        char current_char;
        token_t current_token;
        error_t error;
        symbol_table_t globals;
        symbol_stack_t stack;
    } vm_t;

    /**@brief Initialize VM: reset source, global table & stack */
    extern void vm_init(vm_t *vm);

    // extern bool vm_exec(vm_t *vm);

    /** @brief Get global symbol */
    extern symbol_t *vm_global_get(vm_t *vm, char *name);
    /** @brief Add global symbol */
    extern int       vm_global_add(vm_t *vm, symbol_t *symbol);
    /** @brief Delete global symbol */
    extern symbol_t *vm_global_delete(vm_t *vm, char *name);

    /** @brief Push symbol on top of stack */
    extern int vm_stack_push(vm_t *vm, symbol_t *symbol);
    /** @brief Pop symbol from top of stack */
    extern symbol_t *vm_stack_pop(vm_t *vm);

    extern symbol_t *vm_auto_add_int(vm_t *vm, int value);
    extern int vm_auto_free(vm_t *vm, char *name);
    extern int vm_auto_gc(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _VM_H */
