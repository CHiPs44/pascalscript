#include <stdio.h>

#include "../include/ps_system.h"
#include "../include/ps_vm.h"

#include "../src/ps_vm.c"

#define EMIT(OPCODE)                                                                                                   \
    if (!ps_vm_emit_opcode(vm, OP_ADD))                                                                                \
        return EXIT_FAILURE;

#define LOAD(VALUE)                                                                                                    \
    if (!ps_vm_emit_load(vm, VALUE))                                                                                   \
        return EXIT_FAILURE;

int main(void)
{
    ps_vm *vm;
    ps_value v;
    ps_value *r;

    vm = ps_vm_init();
    if (vm == NULL)
        goto failure;

    // 4 + 5
    v.type = &ps_system_integer.value->data.t;
    v.data.i = 4;
    LOAD(&v);
    v.data.i = 5;
    LOAD(&v);
    EMIT(OP_ADD);
    EMIT(OP_HALT);

    ps_vm_reset(vm);

    // 9?
    r = ps_vm_pop(vm);
    if (r->type != &ps_system_integer.value->data.t || r->data.i != 9)
        goto failure;

    vm = ps_vm_free(vm);
    return EXIT_SUCCESS;

failure:
    vm = ps_vm_free(vm);
    return EXIT_FAILURE;
}