/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include "ps_error.h"
#include "ps_vm_operator.h"
#include "ps_symbol_stack.h"
#include "ps_symbol.h"
#include "ps_vm.h"

bool vm_is_op_test(ps_vm_opcode_t op)
{
    return op == OP_TEST_EQ ||
           op == OP_TEST_NE ||
           op == OP_TEST_GT ||
           op == OP_TEST_GE ||
           op == OP_TEST_LT ||
           op == OP_TEST_LE;
}

bool vm_is_op_arithmetic(ps_vm_opcode_t op)
{
    return op == OP_ADD ||
           op == OP_SUB ||
           op == OP_MUL ||
           op == OP_DIV ||
           op == OP_MOD;
}

bool vm_is_op_bit(ps_vm_opcode_t op)
{
    return op == OP_BIT_NOT ||
           op == OP_BIT_AND ||
           op == OP_BIT_OR ||
           op == OP_BIT_XOR ||
           op == OP_BIT_SHL ||
           op == OP_BIT_SHR;
}

bool vm_is_op_boolean(ps_vm_opcode_t op)
{
    return op == OP_BOOL_NOT ||
           op == OP_BOOL_AND ||
           op == OP_BOOL_OR ||
           op == OP_BOOL_XOR;
}

ps_type_t vm_get_op_binary_type(ps_vm_opcode_t op, ps_type_t a, ps_type_t b)
{
    if (a == PS_TYPE_NIL || b == PS_TYPE_NIL)
        return PS_TYPE_NIL;
    // X <EQ|NE|GT|GE|LT|LE> X => B
    if (vm_is_op_test(op))
        return PS_TYPE_BOOLEAN;
    // X <op> X => X
    if (a == b)
        return a;
    // U <op> I => I / I <op> U => I
    if ((a == PS_TYPE_UNSIGNED && b == PS_TYPE_INTEGER) || (a == PS_TYPE_INTEGER && b == PS_TYPE_UNSIGNED))
        return PS_TYPE_INTEGER;
    // X <op> R => R / R <op> X => R
    if (a == PS_TYPE_REAL || b == PS_TYPE_REAL)
        return PS_TYPE_REAL;
    return PS_TYPE_NIL;
}

/**
 * @brief Execute unary operator
 *
 * @param vm_t *vm
 * @param operator_t operator
 * @return error_t
 */
error_t vm_exec_op_unary(vm_t *vm, ps_vm_opcode_t op)
{
    ps_value_t result;
    symbol_t *a = vm_stack_pop(vm);
    if (a == NULL)
        return RUNTIME_ERROR_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (a->kind == KIND_AUTO)
        vm_auto_free(vm, a->name);
    if (op == OP_NEG || op == OP_BOOL_NOT || op == OP_BIT_NOT)
    {
        switch (op)
        {
        case OP_NEG:
            if (a->value.type == PS_TYPE_INTEGER)
                result.data.i = -a->value.data.i;
            else if (a->value.type == PS_TYPE_REAL)
                result.data.r = -a->value.data.r;
            else
                return RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL;
            break;
        case OP_BOOL_NOT:
            if (a->value.type == PS_TYPE_BOOLEAN)
                result.data.b = !(a->value.data.b);
            else
                return RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL;
            break;
        case OP_BIT_NOT:
            if (a->value.type == PS_TYPE_INTEGER)
                result.data.i = ~a->value.data.i;
            else if (a->value.type == PS_TYPE_UNSIGNED)
                result.data.u = ~a->value.data.u;
            else
                return RUNTIME_ERROR_EXPECTED_INTEGER_OR_UNSIGNED;
            break;
        default:
            break;
        }
        symbol_t *b = vm_auto_add_integer(vm, result.i);
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

error_t vm_exec_op_binary(vm_t *vm, ps_vm_opcode_t op)
{
    ps_value_t result;
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
        if (a->type != PS_TYPE_INTEGER)
            return RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL;
        if (b->type != PS_TYPE_INTEGER)
            return RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL;
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
        symbol_t *c = vm_auto_add_integer(vm, result.i);
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
