/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include "ps_error.h"
#include "ps_operator.h"
#include "ps_symbol_stack.h"
#include "ps_symbol.h"
#include "ps_vm.h"

/**
 * @brief Execute unary operator
 *
 * @param vm_t *vm
 * @param operator_t operator
 * @return error_t
 */
error_t vm_exec_op_unary(vm_t *vm, operator_t op)
{
    value_t result;
    symbol_t *a = vm_stack_pop(vm);
    if (a == NULL)
        return RUNTIME_ERROR_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (a->kind == KIND_AUTO)
        vm_auto_free(vm, a->name);
    if (op == OP_NEG || op == OP_BOOL_NOT || op == OP_BIT_NOT)
    {
        if (a->type != TYPE_INTEGER)
        {
            return RUNTIME_ERROR_EXPECTED_NUMBER;
        }
        result.i = a->value.i;
        switch (op)
        {
        case OP_NEG:
            result.i = -result.i;
            break;
        case OP_BOOL_NOT:
            result.i = (int)(!(bool)result.i);
            break;
        case OP_BIT_NOT:
            result.i = ~result.i;
            break;
        default:
            break;
        }
        symbol_t *b = vm_auto_add_int(vm, result.i);
        if (b == NULL)
            return RUNTIME_ERROR_GLOBAL_TABLE_OVERFLOW;
        if (vm_stack_push(vm, b) == SYMBOL_STACK_ERROR_OVERFLOW)
        {
            vm_auto_free(vm, b->name);
            return RUNTIME_ERROR_STACK_OVERFLOW;
        }
        return RUNTIME_ERROR_NONE;
    }
    return RUNTIME_ERROR_UNKNOWN_UNARY_OPERATOR;
}

error_t vm_exec_op_binary(vm_t *vm, operator_t op)
{
    value_t result;
    symbol_t *b = vm_stack_pop(vm);
    if (b == NULL)
        return RUNTIME_ERROR_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (b->kind == KIND_AUTO)
        vm_auto_free(vm, b->name);
    symbol_t *a = vm_stack_pop(vm);
    if (a == NULL)
        return RUNTIME_ERROR_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (a->kind == KIND_AUTO)
        vm_auto_free(vm, a->name);
    if (op == OP_ADD || op == OP_SUB ||
        op == OP_MUL || op == OP_DIV ||
        op == OP_MOD || op == OP_BIT_AND ||
        op == OP_BIT_OR || op == OP_BIT_XOR ||
        op == OP_BOOL_AND || op == OP_BOOL_OR)
    {
        if (a->type != TYPE_INTEGER)
            return RUNTIME_ERROR_EXPECTED_NUMBER;
        if (b->type != TYPE_INTEGER)
            return RUNTIME_ERROR_EXPECTED_NUMBER;
        switch (op)
        {
        case OP_ADD:
            result.i = a->value.i + b->value.i;
            break;
        case OP_SUB:
            result.i = a->value.i - b->value.i;
            break;
        case OP_MUL:
            result.i = a->value.i * b->value.i;
            break;
        case OP_DIV:
            if (b->value.i == 0)
                return RUNTIME_ERROR_DIVISION_BY_ZERO;
            result.i = a->value.i / b->value.i;
            break;
        case OP_MOD:
            if (b->value.i == 0)
                return RUNTIME_ERROR_DIVISION_BY_ZERO;
            result.i = a->value.i % b->value.i;
            break;
        case OP_BIT_AND:
            result.i = a->value.i & b->value.i;
            break;
        case OP_BIT_OR:
            result.i = a->value.i | b->value.i;
            break;
        case OP_BIT_XOR:
            result.i = a->value.i ^ b->value.i;
            break;
        case OP_BOOL_AND:
            result.i = (int)((bool)(a->value.i) && (bool)(b->value.i));
            break;
        case OP_BOOL_OR:
            result.i = (int)((bool)(a->value.i) || (bool)(b->value.i));
            break;
        default:
            break;
        }
        symbol_t *c = vm_auto_add_int(vm, result.i);
        if (c == NULL)
            return RUNTIME_ERROR_GLOBAL_TABLE_OVERFLOW;
        if (vm_stack_push(vm, c) == SYMBOL_STACK_ERROR_OVERFLOW)
        {
            vm_auto_free(vm, c->name);
            return RUNTIME_ERROR_STACK_OVERFLOW;
        }
        return RUNTIME_ERROR_NONE;
    }
    return RUNTIME_ERROR_UNKNOWN_BINARY_OPERATOR;
}

/* EOF */
