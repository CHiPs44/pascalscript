#ifndef _VM_H
#define _VM_H

#include "symbol_table.h"
#include "symbol_stack.h"

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
extern symbol_t *vm_global_del(vm_t *vm, char *name);

extern int       vm_stack_push(vm_t *vm, symbol_t *symbol);
extern symbol_t *vm_stack_pop (vm_t *vm, symbol_stack_t *stack);

#endif
