#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "symbol.h"
#include "vm.h"

// vm_t vm;

symbol_t default_globals[] = {
    {"_PS_VERSION_", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 0x00000001L}, // 0.0.0.1
    {"_PS_DATE_", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 0x20230805L},    // 5-aug-2023
    {"MAXINT", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 2147483647L},
    //  { "PI"          , KIND_CONSTANT, TYPE_REAL   , sizeof(double), 3.141592653589793 },
};

/**
 * @brief Initialize VM
 *
 */
void vm_init(vm_t *vm)
{
    symbol_table_init(&vm->globals);
    for (int i = 0; i < sizeof(default_globals) / sizeof(default_globals[0]); i += 1)
    {
        symbol_table_add(&vm->globals, &default_globals[i]);
    }
    symbol_stack_init(&vm->stack);
    vm->program = NULL;
    vm->line = -1;
    vm->column = -1;
}

/**
 * @brief Get global
 *
 * @param name normalized
 * @return global or NULL if not found
 */
symbol_t *vm_global_get(vm_t *vm, char *name)
{
    return symbol_table_get(&vm->globals, name);
}

int vm_global_add(vm_t *vm, symbol_t *symbol)
{
    return symbol_table_add(&vm->globals, symbol);
}

symbol_t *vm_global_del(vm_t *vm,  char *name)
{
    return symbol_table_del(&vm->globals, name);
}

int vm_stack_push(vm_t *vm, symbol_t *symbol)
{
    return symbol_stack_push(vm, symbol);
}

symbol_t *vm_stack_pop (vm_t *vm, symbol_stack_t *stack)
{
    return symbol_stack_pop(vm);
}

/* EOF */
