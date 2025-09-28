/*
    This file is part of the PascalScript Pascal vm.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_system.h"
#include "ps_value.h"
#include "ps_vm.h"

bool ps_vm_return_false(ps_vm *vm, ps_error error)
{
    vm->error = error;
    return false;
}

void *ps_vm_return_null(ps_vm *vm, ps_error error)
{
    vm->error = error;
    return NULL;
}

char *ps_vm_get_opcode_name(ps_vm_opcode opcode)
{
    // clang-format off
    switch (opcode & PS_VM_OPCODE_MASK)
    {
    case OP_NOP:    return "NOP";
    case OP_LIT:    return "LIT";
    case OP_JMP:    return "JMP";
    case OP_JPT:    return "JPT";
    case OP_JPF:    return "JPF";
    case OP_CUP:    return "CUP";
    case OP_RET:    return "RET";
    case OP_ENTER:  return "ENTER";
    case OP_EXIT:   return "EXIT";
    case OP_HLT:    return "HALT";
    case OP_DBG:    return "DBG";
    case OP_NEG:    return "NEG";
    case OP_NOT:    return "NOT";
    case OP_ADD:    return "ADD";
    case OP_SUB:    return "SUB";
    case OP_MUL:    return "MUL";
    case OP_DIV:    return "DIV";
    case OP_MOD:    return "MOD";
    case OP_AND:    return "AND";
    case OP_OR:     return "OR";
    case OP_XOR:    return "XOR";
    case OP_SHL:    return "SHL";
    case OP_SHR:    return "SHR";
    case OP_CEQ:    return "CEQ";
    case OP_CNE:    return "CNE";
    case OP_CGT:    return "CGT";
    case OP_CGE:    return "CGE";
    case OP_CLT:    return "CLT";
    case OP_CLE:    return "CLE";
    case OP_SYS:    return "SYS";
    default:        return "???";
    }
    // clang-format on
}

ps_vm *ps_vm_init(uint32_t size)
{
    ps_vm *vm = calloc(1, sizeof(ps_vm));
    if (vm == NULL)
        return NULL;
    vm->code = calloc(size, sizeof(uint8_t));
    if (vm->code == NULL)
        return ps_vm_free(vm);
    vm->stack = ps_value_stack_alloc();
    if (vm->stack == NULL)
        return ps_vm_free(vm);
    vm->size = size;
    vm->used = 0;
    vm->debug = false;
    vm->trace = false;
    vm->range_check = false;
    ps_vm_reset(vm);
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
        vm->stack = ps_value_stack_free(vm->stack);
    }
    free(vm);
    return NULL;
}

void ps_vm_reset(ps_vm *vm)
{
    if (vm->trace)
        fprintf(stderr, "VM\tRESET\n");
    vm->pc = 0;
    vm->error = PS_ERROR_NONE;
}

bool ps_vm_push(ps_vm *vm, ps_value *value)
{
    if (vm->trace)
        fprintf(stderr, "VM\tPUSH %s\n", ps_value_get_debug_string(value));
    if (!ps_value_stack_push(vm->stack, value))
        return ps_vm_return_false(vm, PS_ERROR_STACK_OVERFLOW);
    return true;
}

bool ps_vm_pop(ps_vm *vm, ps_value *value)
{
    if (vm->trace)
        fprintf(stderr, "VM\tPOP\n");
    if (!ps_value_stack_pop(vm->stack, value))
        return ps_vm_return_false(vm, PS_ERROR_STACK_UNDERFLOW);
    return true;
}

bool ps_vm_emit(ps_vm *vm, ps_vm_opcode opcode, ps_value_type type)
{
    if (vm->pc >= vm->size)
        return ps_vm_return_false(vm, PS_ERROR_OVERFLOW);
    uint8_t instruction = (opcode & PS_VM_OPCODE_MASK) | (type & PS_VM_TYPE_MASK);
    if (vm->debug)
        fprintf(stderr, "EMIT1\t%04x %02x %s (used=%u)\n", vm->pc, opcode, ps_vm_get_opcode_name(opcode), vm->used);
    vm->code[vm->pc++] = instruction;
    vm->used += 1;
    if (vm->debug)
        fprintf(stderr, "EMIT2\t%04x %02x %s (used=%u)\n", vm->pc, opcode, ps_vm_get_opcode_name(opcode), vm->used);
    return true;
}

ps_type_definition *ps_vm_get_type_def(ps_vm *vm, ps_value_type type)
{
    // clang-format off
    switch (type)
    {
    case /* 0 */ PS_TYPE_NONE:      return ps_system_none.value->data.t;
    case /* 1 */ PS_TYPE_REAL:      return &ps_system_real;
    case /* 2 */ PS_TYPE_INTEGER:   return &ps_system_integer;
    case /* 3 */ PS_TYPE_UNSIGNED:  return &ps_system_unsigned;
    case /* 4 */ PS_TYPE_BOOLEAN:   return &ps_system_boolean;
    case /* 5 */ PS_TYPE_CHAR:      return &ps_system_char;
    case /* 6 */ PS_TYPE_STRING:    return ps_vm_return_null(vm, PS_ERROR_NOT_IMPLEMENTED);
    default:                        return ps_vm_return_null(vm, PS_ERROR_UNEXPECTED_TYPE);
    }
    // clang-format on
}

uint8_t ps_vm_get_type_size(ps_vm *vm, ps_value_type type)
{
    // clang-format off
    switch (type)
    {
    case /* 1 */ PS_TYPE_REAL:      return sizeof(ps_real);
    case /* 2 */ PS_TYPE_INTEGER:   return sizeof(ps_integer);
    case /* 3 */ PS_TYPE_UNSIGNED:  return sizeof(ps_unsigned);
    case /* 4 */ PS_TYPE_BOOLEAN:   return sizeof(ps_boolean);
    case /* 5 */ PS_TYPE_CHAR:      return sizeof(ps_char);
    case /* 6 */ PS_TYPE_STRING:    return ps_vm_return_false(vm, PS_ERROR_NOT_IMPLEMENTED);
    default:                        return ps_vm_return_false(vm, PS_ERROR_UNEXPECTED_TYPE);
    }
    // clang-format on
}

bool ps_vm_emit_load(ps_vm *vm, ps_value *value)
{
    uint8_t size = ps_vm_get_type_size(vm, value->type->base);
    if (size == 0)
        return false;
    if (vm->pc + 1 + size >= vm->size)
    {
        vm->error = PS_ERROR_OVERFLOW;
        return false;
    }
    uint8_t instruction = OP_LIT | value->type->base;
    if (vm->debug)
        fprintf(stderr, "EMIT1\t%04x %02x %s.%u %s (used=%u, type=%d, size=%d)\n", vm->pc, instruction,
                ps_vm_get_opcode_name(instruction), instruction & PS_VM_TYPE_MASK, ps_value_get_debug_string(value),
                vm->used, value->type->base, size);
    vm->code[vm->pc++] = instruction;
    memcpy(&vm->code[vm->pc], &value->data, size);
    vm->pc += size;
    vm->used += 1 + size;
    if (vm->debug)
        fprintf(stderr, "EMIT2\t%04x %02x %s.%u %s (used=%u, type=%d, size=%d)\n", vm->pc, instruction,
                ps_vm_get_opcode_name(instruction & PS_VM_OPCODE_MASK), instruction & PS_VM_TYPE_MASK,
                ps_value_get_debug_string(value), vm->used, value->type->base, size);
    return true;
}

bool ps_function_unary_op(ps_vm *vm, ps_value *value, ps_value *result, ps_vm_opcode opcode);

bool ps_vm_exec(ps_vm *vm)
{
    uint8_t instruction;
    bool stop = false;
    ps_value left = {0};
    ps_value right = {0};
    ps_value result = {0};
    uint8_t size;

    while (!stop)
    {
        instruction = vm->code[vm->pc];
        if (vm->trace)
            fprintf(stderr, "%04x %s\n", vm->pc, ps_vm_get_opcode_name(instruction));
        vm->pc += 1;
        switch (instruction & PS_VM_OPCODE_MASK)
        {
        case OP_LIT:
            left.type = ps_vm_get_type_def(vm, instruction & PS_VM_TYPE_MASK);
            if (left.type == NULL || left.type->base == PS_TYPE_NONE)
                goto failure;
            size = ps_vm_get_type_size(vm, instruction & PS_VM_TYPE_MASK);
            if (size == 0)
                goto failure;
            memcpy(&left.data, &vm->code[vm->pc], size);
            if (!ps_vm_push(vm, &left))
                goto failure;
            vm->pc += size;
            break;
        case OP_NEG:
        case OP_NOT:
            if (!ps_vm_pop(vm, &left))
                goto failure;
            if (!ps_function_unary_op(vm, &left, &result, instruction & PS_VM_OPCODE_MASK))
                goto failure;
            if (!ps_vm_push(vm, &result))
                goto failure;
            break;
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_MOD:
        case OP_AND:
        case OP_OR:
        case OP_XOR:
        case OP_SHL:
        case OP_SHR:
        case OP_CEQ:
        case OP_CNE:
        case OP_CGT:
        case OP_CGE:
        case OP_CLT:
        case OP_CLE:
            if (!ps_vm_pop(vm, &right))
                goto failure;
            if (!ps_vm_pop(vm, &left))
                goto failure;
            if (!ps_function_binary_op(vm, &left, &right, &result, instruction & PS_VM_OPCODE_MASK))
                goto failure;
            if (!ps_vm_push(vm, &result))
                goto failure;
            break;
        case OP_HLT:
            stop = true;
            break;
        default:
            break;
        }
    }
    return true;
failure:
    return false;
}

void ps_vm_dump(ps_vm *vm, char *title)
{
    uint8_t instruction;
    uint8_t size;
    ps_value value;

    fprintf(stderr, "=== %s ===\n", title ? title : "VM DUMP");
    fprintf(stderr, "PC: %u\n", vm->pc);
    fprintf(stderr, "USED: %u / SIZE: %u\n", vm->used, vm->size);
    fprintf(stderr, "ERROR: %d\n", vm->error);
    fprintf(stderr, "DEBUG: %s\n", vm->debug ? "ON" : "OFF");
    fprintf(stderr, "TRACE: %s\n", vm->trace ? "ON" : "OFF");
    fprintf(stderr, "RANGE_CHECK: %s\n", vm->range_check ? "ON" : "OFF");
    fprintf(stderr, "CODE:\n");
    uint32_t i = 0;
    while (i < vm->used)
    {
        instruction = vm->code[i];
        switch (instruction & PS_VM_OPCODE_MASK)
        {
        case OP_LIT:
            value.type = ps_vm_get_type_def(vm, instruction & PS_VM_TYPE_MASK);
            size = ps_vm_get_type_size(vm, instruction & PS_VM_TYPE_MASK);
            if (value.type != NULL && size != 0)
            {
                memcpy(&value.data, &vm->code[i + 1], size);
                fprintf(stderr, "  %04X %02X %s %s\n", i, instruction, ps_vm_get_opcode_name(instruction),
                        ps_value_get_debug_string(&value));
                i += size;
            }
            break;
        default:
            fprintf(stderr, "  %04X %02X %s (%d)\n", i, vm->code[i], ps_vm_get_opcode_name(instruction),
                    instruction & PS_VM_TYPE_MASK);
            break;
        }
        i += 1;
    }
    ps_value_stack_dump(vm->stack, "STACK:");
    fprintf(stderr, "=== END DUMP ===\n");
}

/* EOF */
