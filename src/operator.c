/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include "symbol.h"
#include "vm.h"
#include "symbol_stack.h"
#include "runtime_error.h"
#include "operator.h"

/**
 * @brief Execute unary operator
 * 
 * @param vm_t *vm
 * @param operator_t operator 
 * @return runtime_error_t 
 */
runtime_error_t vm_exec_op_unary(vm_t *vm, operator_t operator)
{
    symbol_t *a = vm_stack_pop(vm);
    if (a == NULL)
        return RUNTIME_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (a->kind == KIND_AUTO)
        vm_auto_free(vm, a->name);
    switch (operator)
    {
    case OP_NEG:
        if (a->type != TYPE_INTEGER)
        {
            return RUNTIME_EXPECTED_NUMBER;
        }
        symbol_t *b = vm_auto_add_int(vm, -a->value.i);
        if (b == NULL)
            return RUNTIME_GLOBAL_TABLE_FULL;
        if (vm_stack_push(vm, b) == SYMBOL_STACK_OVERFLOW)
        {
            vm_auto_free(vm, b->name);
            return RUNTIME_STACK_OVERFLOW;
        }
        return RUNTIME_OK;
    }
    return RUNTIME_UNKNOWN_UNARY_OPERATOR;
}

runtime_error_t vm_exec_op_binary(vm_t *vm, operator_t operator)
{
    value_t result;
    symbol_t *b = vm_stack_pop(vm);
    if (b == NULL)
        return RUNTIME_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (b->kind == KIND_AUTO)
        vm_auto_free(vm, b->name);
    symbol_t *a = vm_stack_pop(vm);
    if (a == NULL)
        return RUNTIME_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (a->kind == KIND_AUTO)
        vm_auto_free(vm, a->name);
    switch (operator)
    {
    case OP_ADD:
    case OP_SUB:
    case OP_MUL:
    case OP_DIV:
    case OP_MOD:
        if (a->type != TYPE_INTEGER)
            return RUNTIME_EXPECTED_NUMBER;
        if (b->type != TYPE_INTEGER)
            return RUNTIME_EXPECTED_NUMBER;
        if (operator== OP_ADD)
            result.i = a->value.i + b->value.i;
        if (operator== OP_SUB)
            result.i = a->value.i - b->value.i;
        if (operator== OP_MUL)
            result.i = a->value.i * b->value.i;
        if (operator== OP_DIV)
            result.i = a->value.i / b->value.i;
        if (operator== OP_MOD)
            result.i = a->value.i % b->value.i;
        symbol_t *c = vm_auto_add_int(vm, result.i);
        if (c == NULL)
            return RUNTIME_GLOBAL_TABLE_FULL;
        if (vm_stack_push(vm, c) == SYMBOL_STACK_OVERFLOW)
        {
            vm_auto_free(vm, c->name);
            return RUNTIME_STACK_OVERFLOW;
        }
        return RUNTIME_OK;
    }
    return RUNTIME_UNKNOWN_BINARY_OPERATOR;
}

/* EOF */
