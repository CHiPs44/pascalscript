/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_VM_H
#define _PS_VM_H

#include "ps_error.h"
#include "ps_token.h"
#include "ps_symbol_table.h"
#include "ps_symbol_stack.h"

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
        error_t error;
        char *source;
        size_t length;
        unsigned int line_count;
        char **line_starts;
        uint16_t *line_lengths;
        int current_line;
        int current_column;
        char current_char;
        token_t current_token;
        symbol_table_t symbols;
        symbol_stack_t stack;
    } vm_t;

    /**@brief Initialize VM: reset source, global table & stack */
    void vm_init(vm_t *vm);

    // extern bool vm_exec(vm_t *vm);

    /** @brief Get global symbol */
    symbol_t *vm_global_get(vm_t *vm, char *name);
    /** @brief Add global symbol */
    int vm_global_add(vm_t *vm, symbol_t *symbol);
    /** @brief Delete global symbol */
    int vm_global_delete(vm_t *vm, char *name);

    /** @brief Push symbol on top of stack */
    int vm_stack_push(vm_t *vm, symbol_t *symbol);
    /** @brief Pop symbol from top of stack */
    symbol_t *vm_stack_pop(vm_t *vm);

    symbol_t *vm_auto_add_int(vm_t *vm, int value);
    int vm_auto_free(vm_t *vm, char *name);
    int vm_auto_gc(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VM_H */
