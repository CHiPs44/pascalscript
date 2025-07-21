#include <stdio.h>

#include "../include/ps_system.h"
#include "../include/ps_vm.h"

#include "../src/ps_buffer.c"
#include "../src/ps_environment.c"
#include "../src/ps_error.c"
#include "../src/ps_functions.c"
#include "../src/ps_lexer.c"
#include "../src/ps_operator.c"
// #include "../src/ps_parser.c"
#include "../src/ps_procedures.c"
#include "../src/ps_readall.c"
#include "../src/ps_string.c"
#include "../src/ps_string_heap.c"
#include "../src/ps_symbol.c"
#include "../src/ps_symbol_table.c"
#include "../src/ps_system.c"
#include "../src/ps_token.c"
#include "../src/ps_type_definition.c"
#include "../src/ps_value.c"
#include "../src/ps_value_stack.c"
#include "../src/ps_vm.c"

#define EMIT(OPCODE, TYPE)                                                                                             \
    if (!ps_vm_emit(vm, OPCODE, TYPE))                                                                                 \
        goto failure;

#define LOAD(VALUE)                                                                                                    \
    if (!ps_vm_emit_load(vm, VALUE))                                                                                   \
        goto failure;

int main(void)
{
    ps_vm *vm;
    ps_value value = {.type = ps_system_integer.value->data.t, .data.i = 0};
    ps_value result = {.type = ps_system_integer.value->data.t, .data.i = 0};

    ps_environment *environment = ps_system_init();

    // initialize VM
    vm = ps_vm_init(256);
    if (vm == NULL)
        goto failure;
    vm->debug = true;
    vm->trace = true;

    uint8_t i = OP_LIT | PS_TYPE_INTEGER;
    fprintf(stderr, "byte=%02x, opcode=%02x / %s, type=%d / %d\n", i, i & PS_VM_OPCODE_MASK, ps_vm_get_opcode_name(i),
            i & PS_VM_TYPE_MASK, ps_vm_get_type_size(vm, i & PS_VM_TYPE_MASK));

    // 40 + 2
    value.type = ps_system_integer.value->data.t;
    value.data.i = 40;
    LOAD(&value);
    fprintf(stderr, "pc=%04x, used=%d\n", vm->pc, vm->used);

    i = vm->code[0];
    fprintf(stderr, "byte=%02x, opcode=%02x / %s, type=%d / %d\n", i, i & PS_VM_OPCODE_MASK, ps_vm_get_opcode_name(i),
            i & PS_VM_TYPE_MASK, ps_vm_get_type_size(vm, i & PS_VM_TYPE_MASK));

    value.data.i = 2;
    LOAD(&value);
    EMIT(OP_ADD, PS_TYPE_INTEGER);
    EMIT(OP_HLT, PS_TYPE_NONE);

    ps_vm_dump(vm, "VM TEST 00: 40+2=42?");

    // execute "program"
    ps_vm_reset(vm);
    if (!ps_vm_exec(vm))
        goto failure;

    // => 42?
    if (!ps_vm_pop(vm, &result))
        goto failure;
    if (result.type != ps_system_integer.value->data.t || result.data.i != 42)
        goto failure;
    fprintf(stderr, "OK: 40+2=%s\n", ps_value_get_debug_value(&result));

    vm = ps_vm_free(vm);
    ps_system_done();
    ps_environment_done(environment);
    return EXIT_SUCCESS;

failure:
    vm = ps_vm_free(vm);
    ps_system_done();
    ps_environment_done(environment);
    return EXIT_FAILURE;
}