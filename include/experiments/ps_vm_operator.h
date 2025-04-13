/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_OPERATOR_H_
#define _PS_OPERATOR_H_

#include "ps_error.h"
#include "ps_vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

extern ps_error vm_exec_op_unary(ps_vm *vm, ps_vm_opcode op);
extern ps_error vm_exec_op_binary(ps_vm *vm, ps_vm_opcode op);

#ifdef __cplusplus
}
#endif

#endif /* _PS_OPERATOR_H_ */
