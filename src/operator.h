/*
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _OP_H_
#define _OP_H_

typedef enum _operator_t {
    OP_NOP,
    OP_NEG,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_BIT_NOT,
    OP_BIT_AND,
    OP_BIT_OR,
    OP_BIT_XOR,
    OP_BOOL_NOT,
    OP_BOOL_AND,
    OP_BOOL_OR,
    OP_BOOL_XOR,
    OP_END_ENUM
} operator_t;

extern runtime_error_t vm_exec_op_unary(vm_t *vm, operator_t operator);
extern runtime_error_t vm_exec_op_binary(vm_t *vm, operator_t operator);

#endif
