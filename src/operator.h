/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _OPERATOR_H_
#define _OPERATOR_H_

#include "runtime_error.h"
#include "vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

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

extern runtime_error_t vm_exec_op_unary(vm_t *vm, operator_t op);
extern runtime_error_t vm_exec_op_binary(vm_t *vm, operator_t op);

#ifdef __cplusplus
}
#endif

#endif /* _OPERATOR_H_ */
