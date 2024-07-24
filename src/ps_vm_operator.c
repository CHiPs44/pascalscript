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

bool vm_is_op_test(ps_vm_opcode op)
{
    return op == OP_TEST_EQ ||
           op == OP_TEST_NE ||
           op == OP_TEST_GT ||
           op == OP_TEST_GE ||
           op == OP_TEST_LT ||
           op == OP_TEST_LE;
}

bool vm_is_op_arithmetic(ps_vm_opcode op)
{
    return op == OP_ADD ||
           op == OP_SUB ||
           op == OP_MUL ||
           op == OP_DIV ||
           op == OP_MOD;
}

bool vm_is_op_bit(ps_vm_opcode op)
{
    return op == OP_BIT_NOT ||
           op == OP_BIT_AND ||
           op == OP_BIT_OR ||
           op == OP_BIT_XOR ||
           op == OP_BIT_SHL ||
           op == OP_BIT_SHR;
}

bool vm_is_op_boolean(ps_vm_opcode op)
{
    return op == OP_BOOL_NOT ||
           op == OP_BOOL_AND ||
           op == OP_BOOL_OR ||
           op == OP_BOOL_XOR;
}

ps_type vm_get_op_binary_type(ps_vm_opcode op, ps_type a, ps_type b)
{
    if (a == PS_TYPE_NONE || b == PS_TYPE_NONE)
        return PS_TYPE_NONE;
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
    return PS_TYPE_NONE;
}

/**
 * @brief Execute unary operator
 *
 * @param ps_vm *vm
 * @param operator_t operator
 * @return ps_error
 */
ps_error vm_exec_op_unary(ps_vm *vm, ps_vm_opcode op)
{
    ps_value result;
    ps_symbol *a = vm_stack_pop(vm);
    if (a == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (a->kind == SYMBOL_TYPE_AUTO)
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
                return PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL;
            break;
        case OP_BOOL_NOT:
            if (a->value.type == PS_TYPE_BOOLEAN)
                result.data.b = !(a->value.data.b);
            else
                return PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL;
            break;
        case OP_BIT_NOT:
            if (a->value.type == PS_TYPE_INTEGER)
                result.data.i = ~a->value.data.i;
            else if (a->value.type == PS_TYPE_UNSIGNED)
                result.data.u = ~a->value.data.u;
            else
                return PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_UNSIGNED;
            break;
        default:
            break;
        }
        ps_symbol *b = vm_auto_add_integer(vm, result.i);
        if (b == NULL)
            return PS_RUNTIME_ERROR_GLOBAL_TABLE_OVERFLOW;
        if (vm_stack_push(vm, b) == PS_SYMBOL_STACK_ERROR_OVERFLOW)
        {
            vm_auto_free(vm, b->name);
            return PS_RUNTIME_ERROR_STACK_OVERFLOW;
        }
        return PS_RUNTIME_ERROR_NONE;
    }
    return PS_RUNTIME_ERROR_UNKNOWN_UNARY_OPERATOR;
}

ps_error vm_exec_op_binary(ps_vm *vm, ps_vm_opcode op)
{
    ps_value result;
    ps_symbol *b = vm_stack_pop(vm);
    if (b == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (b->kind == SYMBOL_TYPE_AUTO)
        vm_auto_free(vm, b->name);
    ps_symbol *a = vm_stack_pop(vm);
    if (a == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    // Release auto values ASAP, we can still reference them
    if (a->kind == SYMBOL_TYPE_AUTO)
        vm_auto_free(vm, a->name);
    if (op == OP_ADD || op == OP_SUB ||
        op == OP_MUL || op == OP_DIV ||
        op == OP_MOD || op == OP_BIT_AND ||
        op == OP_BIT_OR || op == OP_BIT_XOR ||
        op == OP_BOOL_AND || op == OP_BOOL_OR)
    {
        if (a->value.type != PS_TYPE_INTEGER)
            return PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL;
        if (b->value.type != PS_TYPE_INTEGER)
            return PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL;
        switch (op)
        {
        case OP_ADD:
            result.data.i = a->value.data.i + b->value.data.i;
            break;
        case OP_SUB:
            result.data.i = a->value.data.i - b->value.data.i;
            break;
        case OP_MUL:
            result.data.i = a->value.data.i * b->value.data.i;
            break;
        case OP_DIV:
            if (b->value.data.i == 0)
                return PS_RUNTIME_ERROR_DIVISION_BY_ZERO;
            result.data.i = a->value.data.i / b->value.data.i;
            break;
        case OP_MOD:
            if (b->value.data.i == 0)
                return PS_RUNTIME_ERROR_DIVISION_BY_ZERO;
            result.data.i = a->value.data.i % b->value.data.i;
            break;
        case OP_BIT_AND:
            result.data.u = a->value.data.u & b->value.data.u;
            break;
        case OP_BIT_OR:
            result.data.u = a->value.data.u | b->value.data.u;
            break;
        case OP_BIT_XOR:
            result.data.u = a->value.data.u ^ b->value.data.u;
            break;
        case OP_BOOL_AND:
            result.data.b = a->value.data.b && b->value.data.b;
            break;
        case OP_BOOL_OR:
            result.data.b = a->value.data.b || b->value.data.b;
            break;
        default:
            break;
        }
        ps_symbol *c = vm_auto_add_integer(vm, result.data.i);
        if (c == NULL)
            return PS_RUNTIME_ERROR_GLOBAL_TABLE_OVERFLOW;
        if (vm_stack_push(vm, c) == PS_SYMBOL_STACK_ERROR_OVERFLOW)
        {
            vm_auto_free(vm, c->name);
            return PS_RUNTIME_ERROR_STACK_OVERFLOW;
        }
        return PS_RUNTIME_ERROR_NONE;
    }
    return PS_RUNTIME_ERROR_UNKNOWN_BINARY_OPERATOR;
}

/* EOF */
