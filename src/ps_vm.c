/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_symbol.h"
#include "ps_value.h"
#include "ps_version.h"
#include "ps_vm.h"

ps_vm *ps_vm_init()
{
    ps_vm *vm = calloc(1, sizeof(ps_vm));
    if (vm == NULL)
        return NULL;
    vm->code = calloc(1024, sizeof(uint8_t));
    if (vm->code == NULL)
        return ps_vm_free(vm);
    vm->stack = ps_symbol_stack_init();
    if (vm->stack == NULL)
        return ps_vm_free(vm);
    vm->size = sizeof(vm->code);
    vm->used = 0;
    vm->pc = 0;
    vm->range_check = true;
    return vm;
}

ps_vm *ps_vm_free(ps_vm *vm)
{
    if (vm == NULL)
        return NULL;
    if (vm->code != NULL)
    {
        free(vm->code);
        vm->code = NULL;
    }
    if (vm->stack != NULL)
    {
        ps_value_stack_free(vm->stack);
        vm->stack = NULL;
    }
    free(vm);
    return NULL;
}

void ps_vm_reset(ps_vm *vm)
{
    vm->pc = 0;
    vm->error = PS_ERROR_NONE;
}

bool ps_vm_push(ps_vm *vm, ps_value *value)
{
    return ps_symbol_stack_push(vm->stack, value);
}

ps_value *ps_vm_pop(ps_vm *vm)
{
    return ps_symbol_stack_pop(vm->stack);
}

bool ps_vm_emit(ps_vm *vm, ps_vm_opcode opcode)
{
    if (vm->pc >= vm->size)
        return false;
    ps_vm_instruction instruction = {.instruction.opcode = opcode, .instruction.size = 0};
    vm->code[vm->pc++] = instruction.byte;
    vm->used += 1;
    return true;
}

bool ps_vm_emit_load(ps_vm *vm, ps_value *value)
{
    uint8_t count;
    switch (value->type->base)
    {
    case PS_TYPE_REAL:
        count = sizeof(ps_real);
        break;
    case PS_TYPE_INTEGER:
        count = sizeof(ps_integer);
        break;
    case PS_TYPE_UNSIGNED:
        count = sizeof(ps_unsigned);
        break;
    case PS_TYPE_BOOLEAN:
        count = sizeof(ps_boolean);
        break;
    case PS_TYPE_CHAR:
        count = sizeof(ps_char);
        break;
    case PS_TYPE_STRING:
        return false;
    default:
        return false;
    }
    if (vm->pc + 1 + count >= vm->size)
        return false;
    ps_vm_instruction instruction = {.instruction.opcode = OP_LOAD, .instruction.size = count};
    vm->code[vm->pc++] = instruction.byte;
    memcpy(&vm->code[vm->pc], &value->data, count);
    vm->pc += count;
    vm->used += 1 + count;
    return true;
}

bool ps_vm_exec(ps_vm *vm)
{
    ps_vm_instruction instruction;
    bool stop = false;
    while (!stop)
    {
        instruction.byte = vm->code[vm->pc++];
        switch (instruction.instruction.opcode)
        {
        case OP_HALT:
            stop = true;
            break;
        default:
            break;
        }
    }
    return true;
}

/* EOF */
