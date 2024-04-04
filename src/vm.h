/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _VM_H
#define _VM_H

// #include "loader.h"
#include "symbol_table.h"
#include "symbol_stack.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_ROWS
#define MAX_ROWS 256
#endif

#ifndef MAX_COLS
#define MAX_COLS 256
#endif

typedef struct _vm_t
{
    char *source[MAX_ROWS][MAX_COLS];
    unsigned int max_row;
    unsigned int max_col[MAX_ROWS];
    unsigned int row;
    unsigned int col;
    symbol_table_t globals;
    symbol_stack_t stack;
} vm_t;

extern void vm_init(vm_t *vm);

// extern bool vm_load_file(vm_t *vm, char *filename);

extern bool vm_load_text(vm_t *vm, char *text);

extern bool vm_exec(vm_t *vm);

extern symbol_t *vm_global_get(vm_t *vm, char *name);
extern int       vm_global_add(vm_t *vm, symbol_t *symbol);
// extern symbol_t *vm_global_del(vm_t *vm, char *name);

extern int       vm_stack_push(vm_t *vm, symbol_t *symbol);
extern symbol_t *vm_stack_pop (vm_t *vm);

extern symbol_t *vm_auto_add_int(vm_t *vm, int value);
extern int       vm_auto_free(vm_t *vm, char *name);
extern int       vm_auto_gc(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _VM_H */
