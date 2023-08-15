/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _VM_H
#define _VM_H

#include "symbol_table.h"
#include "symbol_stack.h"

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct _vm_t
{
    char **program;
    unsigned int line;
    unsigned int column;
    symbol_table_t globals;
    symbol_stack_t stack;
} vm_t;

extern void vm_init(vm_t *vm);

extern symbol_t *vm_global_get(vm_t *vm, char *name);
extern int       vm_global_add(vm_t *vm, symbol_t *symbol);
// extern symbol_t *vm_global_del(vm_t *vm, char *name);

extern int       vm_stack_push(vm_t *vm, symbol_t *symbol);
extern symbol_t *vm_stack_pop (vm_t *vm);

extern symbol_t *vm_auto_add_int(vm_t *vm, int value);
extern int vm_auto_free(vm_t *vm, char *name);
extern int vm_auto_gc(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _VM_H */
